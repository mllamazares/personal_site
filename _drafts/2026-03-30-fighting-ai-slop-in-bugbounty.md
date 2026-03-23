---
layout: post
slug: fighting-ai-slop-in-bugbounty
title: fighting AI slop in bug bounty
tags: [bug-bounty, ai, opinion, appsec]
---

A few weeks ago, [Robbe Van Roey](https://www.linkedin.com/feed/update/urn:li:activity:7441023910696882176/) posted something on LinkedIn that resonated with pretty much everyone in the bug bounty space: HackerOne receives around 200 reports *per hour* now. Two hundred. Per hour. Yikes.

His take is that this is unsustainable, and he's right. You can literally point an AI agent at a target, tell it to "hAcK tHiS sItE" and it will produce something that *looks* like a vulnerability report. Chain that with automated submission and zero human oversight, and you've got a firehose of slop with the occasional real finding buried somewhere in the pile.

I already wrote about the individual researcher side of this problem in [how to not be an llm kiddie](https://mll.sh/how-to-not-be-a-llm-kiddie/). But today I want to talk about the *platform* side. What can HackerOne, Bugcrowd, and the rest actually *do* about this?

Because right now, their answer is "fight AI with AI", and that's not enough.

## what hackerone is doing

To their credit, HackerOne isn't ignoring the problem. In July 2025, they launched [Hai Triage](https://www.hackerone.com/platform/triage), an AI-powered system that combines automated classification with human analysts. It filters duplicates, flags out-of-scope submissions, and tries to surface the real stuff faster. By late 2025, 90% of their customers were using Hai in some form.

That's good. Genuinely. AI-assisted triage is the obvious first step.

But it only addresses the *back end* of the pipeline. Reports still flood in. Triagers (human or AI) still have to process them. The fundamental incentive structure hasn't changed: submitting garbage is free, fast, and carries almost no penalty. Getting caught as a slop submitter costs you some reputation points on the platform, and that's about it.[^2]

HackerOne's co-founder Alex Rice has said that they focus on "outcomes, not origins," meaning they don't care *if* you used AI, only *whether* the report is valid. That's a fine principle in theory. In practice, it means the platform has no mechanism to slow down the firehose *before* it hits triage.

## introducing the bug bounty fee

Here's what I think would actually work: a small, conditional submission fee tied to your false positive rate.

The idea isn't new. Tobias Heldt from XOR [floated "Security Report Bonds"](https://thenewstack.io/curl-fights-a-flood-of-ai-generated-bug-reports-from-hackerone/) in the curl discussion, and Stenberg himself agreed it could be a workable model. Bram Cohen (yep, the BitTorrent guy) [wrote a whole post](https://bramcohen.com/p/bug-bounty-submissions-should-require) arguing for submission deposits. The concept keeps popping up because the economics are obvious.

## common objections

### *but what about researchers from low-income countries?*

This is the objection I hear most, and it's a fair one. Bug bounty has been a genuine path to income for talented people in countries where €500 is a month's salary. Adding a paywall, even a small one, could disproportionately affect them.

Two options:
1. **free tier for new accounts.** the first ~10 reports are free while the platform calculates your FP signal. You get a grace period to establish your track record. The fee only kicks in once you've demonstrated a pattern of low-quality submissions.
2. **no fee for newcommers** only apply the fee to accounts that already have a low signal score, not to everyone by default. If you're a new researcher submitting your first reports, you're not affected. If you're account #47382 submitting your 200th hallucinated SSRF, you pay.
3. **fp rate no hacker signal**: some hackers prioritize sending low impact vulns which translate to a low signal. I'm OK with that. Low vulns are legit, if they are paid is because the companies do value them. Not everyone have to send only criticals. The fee should be based on the false positive rate, not on the hacker signal. This avoid penalizing this. Everyone can keep using their own strategy. 

Either way, the goal isn't to gatekeep the industry. It's to make the spray-and-pray approach economically unviable.

### *"won't people just sell to shady brokers instead?"*

The other objection: if it costs money to submit through official channels, why wouldn't someone sell to a third-party broker or, worse, a foreign intelligence agency? Isn't the whole point of bug bounties to *outbid* the black market?

Sure, but let's be realistic about the threat model here. We're not talking about people sitting on critical RCEs in major infrastructure. Those researchers aren't the ones submitting AI slop at scale. The people flooding platforms with garbage reports are doing it because it's *free and easy*, not because they're sophisticated operators weighing their options between HackerOne and the SVR.

And the fee would be *small*. If you've found something real, something worth €2,000 in bounty, putting up €5 as collateral is not a meaningful barrier. 

### *you are ultimately looking for the platform, not for the hackers*

This would ONLY affect to those who submit spam, not everyone.
If the companies and platforms spent that more than expected in triage, the bounties will be lower to cover these costs. 

The fee is a negative incentive, BUT it also can be combined with posisive incentives like adding a bonus who send an EXCELLENT report (Google VRP does that).

## my modest proposal

To condense what I'm trying to say, here's my specific proposal:
1. if your FP rate exceeds X% in your last N reports, you start paying a small fee per submission (5-20€ range).
  - If you have a high overall FP rate in the past, but your last N reports are all valid, you don't pay the fee. This helps AI slopers to detox from the habit.
2. the higher your FP rate, the higher the fee.
3. the fee gets refunded if:
  1. the report is accepted as a valid vulnerability
  2. the report is submitted in good faith (borderline scope, duplicate of an unknown prior report, reasonable but ultimately N/A). For that, the triager *must* objectively justify the false positive determination by referencing the Rules of Engagement: vuln specifically out of scope, N/A with explanation, etc. This isn't a "we reject it and keep your money" scheme. It's a "prove you did the work" filter.

## the benefits

This is not complicated:

- **fewer false positives.** when submitting garbage has a cost, even a tiny one, the volume of pure slop drops dramatically. Spam economics 101.
- **funded triage.** the retained fees from actual false positives go directly toward funding the triage process. The noise literally pays for its own cleanup. This should not impact in the bounty amounts, as the companies will not get that ammount. 
- **better signal-to-noise for everyone.** real researchers get triaged faster because the queue isn't clogged with hallucinated buffer overflows in functions that don't exist.
- **preserved incentives for quality work.** if you're good at this, your FP rate is low, and you never pay a dime. The system is invisible to competent researchers.

A fee won't kill bug bounty. It might save it.


[^1]: Stenberg described it as being "effectively DDoSed" by volunteers. Which is both hilarious and deeply sad.
[^2]: And as we saw with the curl case, some reporters game even that by closing their own reports as "Not Applicable" before the program can mark them, avoiding reputation hits entirely. Beautiful system.
