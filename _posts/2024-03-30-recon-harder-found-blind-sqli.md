---
layout: post
slug: recon-harder-found-blind-sqli
title: recon harder or how I found a hidden blind sqli
tags: [bugbounty, sqli, recon]
---

Found an interesting open port with [naabu](https://github.com/projectdiscovery/naabu), inspected JS files, and discovered an interesting API endpoint. 

After some fuzzing and rebuilding the requests I found in the JS files, I reported an IDOR and this time-based SQLi[^1]:

![simple sqli poc](/assets/img/simple-sqli-poc.png){: loading="lazy"}

[^1]: yep, plain curl, because you don't always need burp :)