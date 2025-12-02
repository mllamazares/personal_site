---
layout: post
slug: passing-oswe-on-the-first-try
title: Passing the OSWE cert on the first try
---

I wanted to share how I passed the OSWE cert on the first try. 

Every know and then I got DMed via linkedin if I could share my experience about this exam.

Considerations:
- I took the exam on january 2025.
- Spoilers: there will be no spoilers here. It's just my personal experience without any clue about the exam or course content.

### about

https://www.offsec.com/courses/web-300/ exam is perhaps the most advanced web penetration testing course along with the CWEE from HackTheBox. 

According to the course description, WEB-300 covers a wide range of web exploitation skills and techniques, including:
- Analyzing and exploiting a deserialization remote code execution (RCE) vulnerability in the DotNetNuke (DNN) platform
- Mastering advanced web security methodologies such as fuzzing, static and dynamic analysis, and manual code review
- Practicing session hijacking techniques to gain unauthorized access to sensitive data and functionality, including exploiting an RCE vulnerability in the Dolibarr application using a dedicated virtual machine

Let me extress that it is a _white-box_ penetration testing course, meaning that you ~have~ must access to the source code to find the vulnerabilities, but also to a dynamic instance so you can test them dynamically.

### background

Skills that i find relevant:
- Penetration testing: I do bug bounty and penetration testing for clients.
- Appsec experience: worked tuning SASTs, DASTs, and SCA tools. I also created appsec CTFs for Secure Code Warrior. 
- scripting experience: I worked for some years y development, both as a programmer and later as systems analyst

Are they a must? I think not, but it helps. I read experiences of people who passed without development experience.
 
### pre-prep

One thing I love to do is before purchasing the course content, try to do research to prepare my own material based on the syllabus, and others experiences. 

This is also a nice way to polish my OSINT skills, lol. To make it even more challenging I try to stick to free resources only. 

I separate the research into two blocks.

#### General vibe
- OffSec documentation
- Medium articles: get a high-level sense of the people.
- Reddit comments to get a sense of the community

#### Topic-specific
Then, I go through the course content and for each topic I search for:
- Articles explaining the concepts
- Tools
- Youtube videos
- Books (optional)
- CTFs
- writeups of CTFs

### prep

Then once I do my research, I create a list of payloads. 

#### resources

Some of the most useful resources I found:

CTFs:
- SecureFlag
- SecureCodeWarrior
- PentesterLab

GitHub repos:
- 

Misc:
- IppSec writeups


### exploits

I created my _\*exploit zoo\*_[^1] which is basically a collection of payloads for each topic. 

![alt text](image.png)


### exam 

I started the exam early in the morning. 

The proctoring was not a hassle and the environment worked like a charm.

I struggled because I missed a vuln because I did not read well the description. 

I slept 3 hours the last day and skipped launch. 

### tips

1. Do the labs including extra miles.
2. Prepare exploits for each vuln type beforehand.
3. Develop a workflow. You must have a plan.
4. Know how to debug. If not, your are cooked. 
5. Read the exam instructions carefully.
6. Seriously, read the exam instructions.


### happy ending

Two days after the exam, I got the email from offsec telling me I passed.

I was a challenging experience, it will make you sweat.


[^1]: I should trademark this someday