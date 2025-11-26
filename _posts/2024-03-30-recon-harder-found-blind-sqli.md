---
layout: post
slug: recon-harder-found-blind-sqli
title: Recon Harder or How I Found a Hidden Blind SQLi
---

Found an interesting open port with [naabu](https://github.com/projectdiscovery/naabu), inspected JS files, and discovered an interesting API endpoint. 

After some fuzzing and rebuilding the requests I found in the JS files, I reported an IDOR and this time-based SQLi [^1]:

![simple sqli poc](/assets/img/simple-sqli-poc.png)

[^1]: plain curl, because you don't always need burp