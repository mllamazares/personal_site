---
layout: post
slug: exfiltrating-a-db-via-blind-sqli-using-python
title: exfiltrating a db via blind sqli using python
tags: [bugbounty, sqli, python]
---

Yesterday, during my bug-hunting journey, I discovered a cool blind SQL injection. To determine if the condition was fulfilled, I had to monitor the request content length. 

I could vibehacked it using sqlmap, but, since I was studiying for OSWE, I coded this simple Python PoC to exfiltrate the database version.

![blind sqli script poc](/assets/img/blind-sql-injection-poc-code.png)

And the output:

![blind sqli script output](/assets/img/blind-sql-injection-poc-output.png)

