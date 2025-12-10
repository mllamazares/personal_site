---
layout: post
slug: using-node-graphs-for-subdomain-relationship-mapping
title: Using Node Graphs for Subdomain Relationship Mapping
tags: [recon, charts, subdomains]
---

I ~~built~~ vibecoded a simple app that lets you visualize subdomain relationships using a dynamic, drag-and-drop node network chart (everything runs locally via 3D.js, so no data ever leaves your browser).

![subs nodes](/assets/img/subs-nodes.png)

AFAIK, the latest version of [OWASP amass](https://github.com/owasp-amass/amass) doesn’t support generating charts anymore. _*play sad_trumpet.mp3_ 🎺

This feature is already integrated in [aquatone](https://github.com/michenriksen/aquatone), but I wanted something more flexible and simpler (just the subs, no other stuff) where I could just paste the merged results from passive and active sub enumeration.

![aquatone node chart](/assets/img/aquatone-node-chart.png)

Honestly, it’s probably cooler than it is useful, but it could make for a neat addition to a report.

**edit**: I stumbled upon [flowsint](https://www.flowsint.io/), which does something similar and much cooler. 

![flowsint dashboard](/assets/img/hero-dark.webp)

I still need to check what export options it has and, more importantly, whether it can be fed from different tools, which was the main reason for creating an ad-hoc tool.