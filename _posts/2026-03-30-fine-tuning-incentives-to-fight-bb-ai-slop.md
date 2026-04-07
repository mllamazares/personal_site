---
layout: post
slug: fine-tuning-incentives-to-fight-bb-ai-slop
title: fine-tuning incentives to fight bug bounty AI slop
tags: [bugbounty, ai, consulting]
---

Recently, [Robbe Van Roey](https://www.linkedin.com/feed/update/urn:li:activity:7441023910696882176/) posted something on linkedin that resonated with pretty much everyone in the bug bounty space: HackerOne receives around 200 reports *per hour* now. Two hundred. Per hour. 

His take is that this is unsustainable, and he's right. You can literally point an AI agent at a target, tell it to _"hAcK tHiS sItE"_ and it will produce something that *looks* like a vulnerability report. Chain that with automated submission and zero human oversight, and you've got a firehose of slop with the occasional real finding buried somewhere in the pile. 🗑️

I already wrote about the individual researcher side of this problem in [how to not be an llm kiddie](https://mll.sh/how-to-not-be-a-llm-kiddie/). But today I want to talk about the *platform* side. What can HackerOne, Bugcrowd, and the rest actually *do* about this?

Because right now, their answer is _"fight AI with AI"_, and, IMHO, that's not enough.

## what h1 is doing

To their credit, HackerOne isn't ignoring the problem. In July 2025, they launched [Hai Triage](https://www.hackerone.com/platform/triage), an AI-powered system that combines automated classification with human analysts. It filters duplicates, flags out-of-scope submissions, and tries to surface the real stuff faster. By late 2025, 90% of their customers were using Hai in some form.

That's good. Genuinely. AI-assisted triage is the obvious first step. 🤖

h1's co-founder Alex Rice has said that they focus on "outcomes, not origins", meaning they don't care *if* you used AI, only *whether* the report is valid. That's a fine principle in theory. In practice, it only addresses the *back end* of the pipeline. Reports still flood in. Triagers (human or AI) still have to process them. 

The fundamental incentive structure hasn't changed: submitting garbage is free, fast, and carries almost no penalty. Getting caught as a slop submitter costs you some reputation points on the platform, and that's about it[^2].

## introducing the bug bounty fee

Here's what I think would actually work: a small, conditional submission fee tied to the false positive rate of the researcher.

The idea isn't new. Tobias Heldt from XOR [floated "Security Report Bonds"](https://thenewstack.io/curl-fights-a-flood-of-ai-generated-bug-reports-from-hackerone/) in the curl discussion, and Stenberg himself agreed it could be a workable model[^1]. Bram Cohen (yep, the BitTorrent guy) [wrote a whole post](https://bramcohen.com/p/bug-bounty-submissions-should-require) arguing for submission deposits. 

The concept keeps popping up because the economics are obvious. 

## addressing common objections

> *"wOn'T pEoPlE jUsT sElL tO sHaDy BrOkErS iNsTeAd?"*

The concern is that if it costs money to submit through official channels, why wouldn't someone sell to a third-party broker or, worse, a foreign intelligence agency? Isn't the whole point of bug bounties to *outbid* the black market?

Sure, but the fee would be *small*. If you've found something real, something worth $2K in bounty, putting up $5 as collateral is not a meaningful barrier. 

So this is not a legit concern. Next.

> *bUt WhAt AbOuT rEsEaRcHers FrOm LoW-InCoMe CoUnTrIeS?*

This is the objection I hear most, and it's a fair one. Bug bounty has been a genuine path to income for talented people in countries where $500 is a month's salary. Adding a paywall, even a small one, could disproportionately affect them.

Three ideas to overcome that:
1. **no fee for newcommers**: the first N reports are free while the platform calculates your FP signal. You get a grace period to establish your track record. The fee only kicks in once you've demonstrated a consistent pattern of low-quality submissions.
2. **fp rate, not hacker signal**: some researchers prioritize low-impact vulns, which drags down their signal. That is fine. Low-impact bugs are legit; if companies pay for them, it is because they see value. The fee should track the FP rate instead of the signal. This lets everyone stick to their own strategy.
3. **geographic parity**: $5 in the US is not $5 in India. The fee should be sensitive to the country of residence of the researcher. This prevents the cost from being a rounding error for some while being a barrier to entry for others.

Either way, the goal isn't to gatekeep the industry. It's to make the spray-and-pray approach economically unviable.

These three specific proposals address that objection. Next.

> *yOu ArE uLtImAtElY lOoKiNg FoR tHe PlAtFoRm, NoT fOr ThE hAcKeRs*

I insist: this would \*only\* affect those who submit spam, not everyone. If companies and platforms spend more than expected on triage, bounties will drop to cover those costs, or they'll ultimately close their programs.

The fee is a *negative* incentive, but it can be combined with *positive* incentives too, like adding a bonus for whoever sends an excellent-written report. Google VRP [already does that](https://bughunters.google.com/blog/level-up-your-reports-introducing-our-updated-report-quality-framework).

## my modest proposal

To condense my point, here is the specific proposal:

1. **application conditions**:
  * if your FP rate exceeds X% in your last N reports, you start paying a small fee per submission (5-20€ range).
  * if you have a high overall FP rate but your last N reports are valid, the fee is waived[^3].
2. **fee calculation**:
  * the higher the FP rate, the higher the fee.
  * the fee should be sensitive to the researcher's country.
  * the fee must align with the potential bounty. Locking $20 for a $50 bounty is not reasonable.
3. **the fee gets refunded if**:
  * the report is accepted as a valid vulnerability.
  * the report is submitted in good faith (e.g., borderline scope, duplicate, or reasonable but ultimately N/A). To keep the money, the triager must objectively justify the FP determination by referencing the Rules of Engagement[^4].

Also, note I've focused on a _platform-level_ approach to fight AI slop, but can apply measures to other layers of the workflow, like defining clear scopes and fallbacks (@companies) or using tools like [Hai](https://www.hackerone.com/resources/hai/hai-agentic-report-assistant) or [NoiseGate](https://github.com/sgmurphy/NoiseGate) to double-check (@hunters).

## the benefits

IMO, pretty obvious:

1. **fewer false positives**: when submitting garbage has a cost, even a tiny one, the volume of pure slop drops dramatically. Spam economics 101.
2. **funded triage**: the retained fees from actual false positives go directly toward funding the triage process. The noise literally pays for its own cleanup. 
3. **better signal-to-noise for everyone**: real researchers get triaged faster because the queue isn't clogged with *critical* self-XSS via paste injections and CSRFs on public newsletter signup forms.
4. **preserved incentives for quality work**: if you're good at this, your FP rate is low, and you never pay a dime. The system is invisible to competent researchers.
5. **reinsertion in mind**: if llm kiddies start sending legit reports, they will improve their FP rate. This is calculated based on the last N reports, not overall. This doesn't condemn someone to _The Eternal Fee_. Reinsertion is possible.

## bottom line

Is this system perfect? No. Did I miss something? I bet I did[^5]. 

But we need to compare potential solutions to their alternatives, and the current strategy is clearly not the best one.

A symbolic fee won't kill bug bounty. It might actually save it.


[^1]: Stenberg described it as being "effectively DSoSed" by volunteers. Which is both hilarious and deeply sad.
[^2]: and as we saw with the curl case, some reporters game even that by closing their own reports as N/A before the program can mark them, avoiding reputation hits entirely. Beautiful system.
[^3]: this helps AI slopers detox from the habit.
[^4]: this isn’t a "reject and keep the cash" scheme. It’s a "prove you did the work" filter.
[^5]: send me an email with your best insults. I have thick skin. 🥊
