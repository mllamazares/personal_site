---
layout: post
slug: passing-osed
title: my experience passing the osed cert
tags: [cert, pentesting]
---

## os-what?

## the course



### stuff i liked
- hands-on
- excellent mentoring support via discord.

### stuff i didn't \*love\*
- only 32bits
- ida pro but it's ida free, lol.

## reqs

## prep


### don't be an oscp-monkey
I stumbled upon these post from dreg.

Basically he points out four common mistakes people make when exploiting BoF:
1.
2.
3.
4.

It is tempting to ask claude how to whipe your a**, but don't. AI is super useful, but do NOT delegate your understanding. Don't copy paste blindly.

## r4nd0m tips
- if you work with sockets, make sure to close them between each operation for stability.

- be confortable  reading rcf to know protocol structure, format and field.
- fuzz from smaller to larger payloads. Review with IDA the controls. For isntance: large to be buff overflow but smaller to pass the filters.
- start from windbg and go to IDA. know when to switch between them.
- make sure IDA and windgb have the same offset.
- calc the offset with rp++ with powerhell to get the library you want.
- When choosing modules make sure they are not dynamic. Although they don't have ASLR.

## resources

- use offsec discord. linking your student account. they have an SLA of 15mins to answer your questions. And they are pretty good, IMO.
- the book
- corelan.be
- Buffer Overflows Made Easy TCM series. Basic stuff. But decent script-kiddie introduction.
https://www.youtube.com/watch?v=qSnPayW6F7U&list=PLLKT__MCUeix3O0DPbmuaRuR_4Hxo4m3G&index=1

## exam

#### timeline


## wh00t wh00t

