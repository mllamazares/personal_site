---
layout: post
slug: sqli-column-enumeration-waf-bypass
title: proving a blind sqli when everything is against you
tags: [sqli, bugbounty, waf]
---

This is the story of how [abfe](https://www.linkedin.com/in/kareem-abfe-b27454347/) and I turned a closed-as-informational report into a critical finding with max bounty, by using column name validation as a boolean oracle and AWS API Gateway to dodge Cloudflare's rate limiting. 🏎️

Let's get to it.

### sniff, sniff... that's smells like a sqli

The endpoint was a coupon listing page with a `keyword` search parameter. Classic stuff. We started poking at it:

```http
GET /coupons/coupon_list?keyword=HERE' HTTP/2
```
→ **503** (broken query, the server choked on the unmatched quote)

```http
GET /coupons/coupon_list?keyword=HERE'||' HTTP/2
```
→ **200 OK** (query executes normally, string concatenation closes the quote)

That 503/200 differential is textbook injection behavior. The single quote breaks the SQL syntax, and `'||'` fixes it by concatenating an empty string. The query runs, the server is happy, and I'm happy too.

To avoid the risk of a duplicate, we submitted the report quickly. It was closed as **informational**. 🫠

Fair enough. A syntax-level differential alone isn't proof of exploitable injection. You need to show you can actually interact with the database.

### hitting the (fire)wall

So basically we needed to escalate from _"this probably breaks SQL"_ to _"dude, we can query your database"_. The standard playbook for blind SQLi is:

- **boolean-based**: inject a condition, observe different responses for true vs. false
- **time-based**: inject `SLEEP(5)` or equivalent, measure response times

Both paths were blocked. Ew.

The `keyword` parameter had *zero observable effect on the page output*. Same HTML, same content, same everything, regardless of what value you passed. No "welcome back" message, no result count, no subtle DOM change. Nothing to diff. Boolean-based was out[^1].

Time-based payloads like `SLEEP()` or `BENCHMARK()` were getting caught by the WAF. We tried the usual evasion tricks, inline comments, case alternation, encoding... No luck[^2].

The injection was almost certainly there, but we couldn't *prove* it.

### the hack: column names as a boolean oracle

After banging our heads for a while, we had an idea: what if we stopped trying to extract data and instead used the database's *own schema* as our oracle?

The logic was dead simple: if we inject an `AND` condition that references a column name, the database itself will tell us whether that column exists:

- **invalid column** → the SQL engine throws an error → **503**
- **valid column** → the query executes normally → **200**

That is:

```http
GET /coupons/coupon_list?keyword=HERE'+AND+testing='1 HTTP/2
```
→ **503** (`testing` is not a real column, SQL error)

```http
GET /coupons/coupon_list?keyword=HERE'+AND+usage_limit='1 HTTP/2
```
→ **200** (`usage_limit` exists in the table, query runs fine)

That's it. Just a plain `AND column_name='1'` that looks completely benign from the WAF's perspective. The database does the validation for us: if the column exists, the condition is syntactically valid and the query runs. If it doesn't, the whole thing explodes.

We had our boolean oracle. Let's escalate it. 🤓

### dodging cloudflare with IP rotation

Now we needed to fuzz, baby. We had to throw a wordlist of common column names at this endpoint and see which ones came back 200. Problem: any competent WAF would denylist our IP after a handful of suspicious-looking requests.

This is where the [IP Rotate](https://portswigger.net/bappstore/2eb2b1cb1cf34cc79cda36f0f9019874) Burp extension by [Rhino Security Labs](https://rhinosecuritylabs.com/aws/bypassing-ip-based-blocking-aws/) comes in. It spins up AWS API Gateway endpoints across multiple regions and routes your Burp traffic through them. Every request comes from a different AWS IP, which have decent reputation scores and rarely get flagged[^3].

We loaded up Intruder with a wordlist of common database column names (`id`, `name`, `email`, `password`, `created_at`, `updated_at`, `status`, you know the drill) and let it rip:

![full chain](/assets/img/waf-ratelimit-fuzz1.png)

As you can see, existing columns returned 200:

![full chain](/assets/img/waf-ratelimit-fuzz2.png)

### going the extra mile

The generic wordlist gave us a few hits, but we wanted more. We went through the entire application and built a custom wordlist with column names that were specific to this app's domain. Coupons, users, transactions, whatever terminology the app used in its frontend, we turned it into candidate column names (`column_name`, `columnname`, `col_name`, etc.).

We ran the fuzz again with the custom wordlist. More hits.

### the final veredict

We submitted a new report with a full enumeration of every column name we'd confirmed and the triagers reached out to the dev team, who confirmed the columns were indeed real.

Since this was the company's most critical asset, it got triaged as **critical** and we landed the maximum bounty:

![full chain](/assets/img/waf-ratelimit-reward.png)

I guess the takeway is that sometimes the answer isn't a fancier payload, but a simpler one. 

Don't try *harder*, try *smarter*. 🧠

[^1]: we probably didn't have enough permissions to generate data that would have been filtered by this endpoint. And/or it might have just been a logging field. Who knows.
[^2]: I bet it was not *impossible*, but at least not *straightforward*.
[^3]: because half the internet’s legitimate traffic comes from AWS anyway, kek.