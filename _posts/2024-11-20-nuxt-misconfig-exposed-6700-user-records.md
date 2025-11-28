---
layout: post
slug: nuxt-misconfig-exposed-6700-user-records
title: Nuxt Misconfig Exposed +6,700 User Records
tags: [bugbounty, idor, pii]
---

Today I found sensitive info disclosure of +6,700 users!

In this case, the Nuxt context object stored PII of all registered users (name, surname, email, organization, role, etc.), making it accessible to anyone curious enough to check the source code. Obviously, this information should be stored server-side.

In the PoC below, you can see a sample of the emails retrieved and the total volume of users disclosed. 

![nuxt pii disclosure](/assets/img/nuxt-pii-disclosure.png)

The organization is already working on a fix.

