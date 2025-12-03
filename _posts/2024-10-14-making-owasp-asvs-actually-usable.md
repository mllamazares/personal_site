---
layout: post
slug: making-owasp-asvs-actually-usable
title: Making OWASP ASVS Actually Usable
tags: [appsec, asvs, owasp]
---

I've always liked [OWASP Application Security Verification Standard (ASVS)](https://owasp.org/www-project-application-security-verification-standard/). It's a solid blueprint of security controls. But try to take it straight off the shelf and implement it in a real product, and you'll quickly feel like you've been handed a map with no roads. _\*play sad_violin.mp3_ 🎻

### exploratory categorization

A while back, I spent some time sorting through ASVS controls to make them actionable. I didn't do anything revolutionary, just a little classification to make my life easier. 

Here's what I looked at:

  1. **DOs vs DON'Ts**. Some controls are proactive, some are preventative. A DO: _"Show a password strength meter to help users pick stronger passwords" (V2.1.8)_. A DON'T: _"Don't limit allowed characters in passwords" (V2.1.9)_. Simple distinction, but it changes how you communicate priorities.
  2. **Who does it**. Devs? Infra? Design? Operations? Knowing who owns the work saves a lot of finger-pointing.
  3. **How you verify it**. SAST, DAST, SCA, code review, pentest, etc. 
  4. **Who verifies it**. QA, offensive team, devs, your grandma, etc. Avoid assumptions.
  5. **Implementation complexity**. Not all controls are equal. Checking password length is trivial; comparing it against a breached passwords list is a bigger lift. Labeling like "hours", "days" or "weeks" is enough.
  6. **Verification complexity**. Same deal. 

It's perfect? No. But it's better than trying to apply them raw [^1].

### folding it to the context

Most ASVS controls are technical, which is enough for some close-ended scenarios. However, if you want requirements to actually drive behavior, you need to fold in functional context: components, roles, exceptions, workflows, business impact, etc. User stories are your friends. I love [this approach by Mario Platt](https://github.com/OpenSecuritySummit/project-ASVS-User-Stories) that uses [Gherkin](https://cucumber.io/docs/gherkin/reference) syntax. 

They need to be digestible by everyone involved in the process: business people, QA, devs, pentesters, etc. Otherwise, your requirements will be just _words_ [^2].

That said, most L1 controls require minimal context and should be enforced and treated as _basic hygiene_. For instance, _TLS for all client connections (V9.1.1)_. If your threat model comes back with _"hey guys, use HTTPS instead of HTTP"_ people will (understandably) roll their eyes [^3]. Duh. 

Also, take into account that it's not that complex to implement V2.1.7 for the first time or the N-th time. Context is key.

### be flexible

Ultimately, you need to _leverage what you have_. Meaning that rather than creating a new workflow you should use the process and tools you already have in place. That will reduce friction and cognitive load. No SAST? Use code review. Teams differ? Create a RASCI matrix for alignment. You get the idea. 

In other words, this categorization must be tailored and _\*alive\*_.

### tooling

There are some tools I found useful to ease the implementation and validation of ASVS controls:
  - [OWASP ASVS Security Evaluation Templates with Nuclei](https://github.com/OWASP/www-project-asvs-security-evaluation-templates-with-nuclei)
  - [OWASP ASVS User Stories](https://github.com/OpenSecuritySummit/project-ASVS-User-Stories)
  - [OWASP ASVS Testing Guide](https://github.com/BlazingWind/OWASP-ASVS-4.0-testing-guide)
  - [ASVS-Agile-Delivery-Guide](https://github.com/mario-platt/ASVS-Agile-Delivery-Guide)
  - [STRIDE-vs-ASVS](https://github.com/mllamazares/STRIDE-vs-ASVS)
  - [AppSec ASVS Bot](https://github.com/neo4j-examples/appsec-asvs-bot)


### bottom line

I pitched this approach in the OWASP Slack ASVS channel. They weren't convinced; they preferred to keep the standard minimal and context-independent. Fair enough. But in practice, a little context makes a lot of difference.

<br>

[^1]: aka _nirvana fallacy_, the informal fallacy of comparing actual things with unrealistic, idealized alternatives.
[^2]: I'm quite bullish about the use of LLMs for this use case.
[^3]: Note that precisely this set of controls are the most easy to automate.