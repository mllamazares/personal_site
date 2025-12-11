---
layout: post
slug: passing-oswe
title: my experience passing the OSWE cert
tags: [cert, oswe, pentesting]
---

After some months, I finally decided to share my experience with the AWAE course and how I passed the OSWE cert on the first try.

Every now and then I get DMed on linkedin asking about this exam. Hopefully this will help me redirect people to this blog every time I get asked again, get some organic traffic, monetize with google ads, throw it into sh*tcoins, and eventually retire to a remote japanese village. 🍜⛩️

**spoiler**: there will be no *spoilers* here. It's just my personal experience without anything that isn’t already public.

### os-what?

The [Offensive Security Web Expert (OSWE)](https://www.offsec.com/courses/web-300/) from OffSec is probably the most advanced web penetration testing cert along with the [Certified Web Exploitation Expert (CWEE)](https://academy.hackthebox.com/preview/certifications/htb-certified-web-exploitation-expert) from HackTheBox.

[WEB-300: Advanced Web Attacks and Exploitation (AWAE)](https://www.offsec.com/courses/web-300/) is the course behind the OSWE certification and covers a wide range of web exploitation skills and techniques, including:

* analyzing and exploiting a deserialization remote code execution (RCE) vulnerability in the DotNetNuke (DNN) platform
* mastering advanced web security methodologies such as fuzzing, static and dynamic analysis, and manual code review
* practicing session hijacking techniques to gain unauthorized access to sensitive data and functionality, including exploiting an RCE vulnerability in the Dolibarr application using a dedicated virtual machine

Let me stress that it is a *white-box* penetration testing course, meaning you ~~should~~ must review the source code to find vulnerabilities, but also to a dynamic instance so you can validate them. You need to excel at both. Plus, you have to write an exploit that automates the whole process. 

### reqs

From my experience, these skills matter:

* *dynamic black-box testing*: web pentesting, bug bounty, or CTF background.
* *secure code reviews*: knowing what a vulnerability looks like from the code perspective. In my case, having worked on tuning SAST tools helped a lot.
* *scripting*: you ultimately need to create working exploits in your language of choice.
* *development*: not required to know how to build full apps in every language, but it definitely helps.

### pre-prep

Before buying any course I like to do my own research and prepare notes based on the [official syllabus](https://www.offsec.com/courses/web-300/download/syllabus) and real people's experiences.

It’s also a nice excuse to polish my osint skillz, lol. And to make it more challenging I try to stick to free resources.

I split my research into two buckets:

* *general vibe*
  * official OffSec docs
  * Medium articles and personal blogs to get a sense of how people approached it
  * Reddit boards like [r/oswe](https://www.reddit.com/r/OSWE/) or [r/codereview](https://www.reddit.com/r/codereview/)
  * books related to the topic

* *topic specific*: I go through the course content and for each topic I search for:
  * articles explaining the concepts
  * relevant tools
  * YouTube videos
  * CTFs
  * sample exploits from [Exploit-DB](https://www.exploit-db.com/)
  * instances of that vuln in different languages (java, C#, python, js, etc.)

### prep

Once I felt comfortable with the topics, I bought the AWAE course. My strategy:

1. read the huge course PDF[^1]. Thoroughly, underlining the important stuff.
2. do the course labs and some of the extra miles
3. complement the course with extra research and CTFs
4. create my *own* notes and exploits

Overall, I liked how the AWAE course was structured. You *learn by pwning* throught specific case studies. It doesn’t dump the solution on you but shows the thought process step by step. Something doesn't work, then it explains why and how to break it. 

This approach works because you see everything in context, and that helps you to develop a good intuition. 

### notes

I used Notion for notetaking[^2] and started categorizing vulns like below, but it ended up quite messy[^3]:
![Failed first structure](/assets/img/failed_exploit_zoo.png)

So I created what I called the *Exploit Zoo🦒*[^4], basically a collection of payloads and exploits for each topic, neatly categorized and tagged: 

![Exploit zoo](/assets/img/exploit_zoo.png)

I also organized the writeups of each lab with the working code like this:

![OSWE labs writeups](/assets/img/oswe_labs_writeup.png)

Sure, I know you can code everything from scratch during the exam, but having a solid starting point saves you precious minutes when the clock is ticking.

### exam

After almost three months in the course, I decided to take the exam. You get 48 hours to break into two machines and 24 hours to write the report.

I kicked it off early in the morning. The onboarding was painless, the proctoring experience was smooth, and the environment behaved decently.

The first hours went well and I landed an admin bypass. I took a walk to cool off. A few hours later I popped RCE and felt like a god.

Then I got ~~humbled~~ stuck for a while, so I had no option but to listen to some metal and go full [Michael Burry mode](https://www.youtube.com/watch?v=yLvZKxF43tQ). 🤘

The real problem was me skimming the exam description and missing a key detail. Once I caught that, I tested my idea dynamically and it worked. After that it was *just* building the exploit.

TBH, the whole thing was tougher than I expected. Slept three hours, skipped lunch, *cried harder*, pushed through, and finally grabbed the flag in the last hour.

Next day I cleaned up the notes, sorted the screenshots, and wrote the report following the required format. Used the default OffSec DOCX template this time, but I'll probably switch to [SysReptor](https://docs.sysreptor.com/offsec-reporting-with-sysreptor/) in the future. 

### tips

1. pwn all labs including extra miles
2. prepare exploits for each vuln type *beforehand*
3. develop your *own* methodology, you need a plan
4. know how to debug, if not you are cooked
5. read the exam instructions carefully
6. seriously, read the freaking exam instructions

### resources

Here is a non-exhaustive list of resources I found useful throughout the journey:

CTFs:
- [SecureFlag](https://knowledge-base.secureflag.com/vulnerabilities/cross_site_scripting/cross_site_scripting_vulnerability.html)
- [SecureCodeWarrior](https://www.securecodewarrior.com/)
- [PentesterLab](https://pentesterlab.com/exercises/codereview/course)
- [PortSwigger Academy](https://portswigger.net/web-security/all-topics)
- [VulnHub](https://www.vulnhub.com/entry/securecode-1,651/)

github repos:
- oswe specific:
  - [Xcatolin/OSWE-Prep](https://github.com/Xcatolin/OSWE-Prep/tree/main)
  - [shreyaschavhan/oswe-awae-pre-preperation-plan-and-notes](https://github.com/shreyaschavhan/oswe-awae-pre-preperation-plan-and-notes)
  - [snoopysecurity/OSWE-Prep](https://github.com/snoopysecurity/OSWE-Prep)
  - [mishmashclone/timip-OSWE](https://github.com/mishmashclone/timip-OSWE?tab=readme-ov-file)
  - [wetw0rk/AWAE-PREP](https://github.com/wetw0rk/AWAE-PREP/tree/master)
  - [rizemon/exploit-writing-for-oswe](https://github.com/rizemon/exploit-writing-for-oswe)
  - [jorgectf/awae-oswe-preparation-resources](https://jorgectf.gitbook.io/awae-oswe-preparation-resources/)
  - [enderphan94/Pre-OSWE](https://github.com/enderphan94/Pre-OSWE/wiki#tips-for-exam)
- other:
  - [absoluteappsec/handouts](https://github.com/absoluteappsec/handouts/tree/master): IMO, these guys have the best E2E secure code review methodology out there. Also found [here](https://absoluteappsec.com/blog/Secure-Code-Review/). Pure gold.
  - [brutuspt/0click_HTB](https://github.com/brutuspt/0click_HTB/tree/master)
  - [elqal3awii/WebSecurity-Academy-with-Python](https://github.com/elqal3awii/WebSecurity-Academy-with-Python)
  - [VoorivexTeam/white-box-challenges](https://github.com/VoorivexTeam/white-box-challenges)

misc:
- [Youtube - @TJ_Null’s OSWE Prep](https://www.youtube.com/playlist?list=PLidcsTyj9JXKTnpphkJ310PVVGF-GuZA0)
- [OWASP_Code_Review_Guide_v2.pdf](https://owasp.org/www-project-code-review-guide/assets/OWASP_Code_Review_Guide_v2.pdf)
- [WSTG - Stable OWASP Foundation](https://owasp.org/www-project-web-security-testing-guide/stable/)
- [HTB and Vulnhub: An OSWE Approach](https://klezvirus.github.io/Misc/HTB-VH-OSWE/)
- [IppSec writeups](https://ippsec.rocks/)

additional tools:
- [BurpSuite Extension - Copy As Python-Requests](https://portswigger.net/bappstore/b324647b6efa4b6a8f346389730df160)
- [curlconverter](https://github.com/curlconverter/curlconverter)


### wh00t wh00t

Fast forward two days, I got the email from OffSec telling me I passed. 🎉

![OSWE passed](/assets/img/owse_passed.png)

The big takeaway, and IMHO what separates a junior from a senior web pentester, is the *ability to chain vulns*.

To do that you need to understand the vulnerability in depth: what it is, why it happens, how to exploit it, what possibilities it opens, whether there's another vuln you can combine to maximize impact, and so on.

As a friend of mine likes to say [^6]:
> you cannot avoid lose your time, without having lost your time in the past

Meaning that only by practicing you develop a sense of what is productive and what is not. In this exam, you will have tons of ideas, but there are plenty of possibilities (and rabbit holes), and you won't have time to validate every hypotesis that comes to mind. You have to *prioritize* based on your methodology and your gut.

Overall, the OSWE was a tough but rewarding experience. It’ll make you sweat, and it gives you a solid foundation for tackling complex white-box pentests. I do recommend it.

<br>

[^1]: used Goodnotes app on my iPad
[^2]: I plan to move to Obsidian like the grown ups
[^3]: note this was a work in progress; some categories or classifications don’t even make sense.
[^4]: I should trademark this someday
[^5]: this guy plays 4D chess
[^6]: this deserves a tattoo or something