---
layout: post
slug: passing-osed
title: my experience passing the osed cert
tags: [cert, pentesting]
---

I recently passed the OSED exam on the first attempt, and it has been the most challenging exam I've taken so far. 

I have a solid programming background, and I'm comfortable reading code (read my OSWE cert review). However, my asm and windows internals knowledge was hello world. 

Here I'll share my personal experience preparing for this exam.

> [!NOTE] spoiler
> There will be no *spoilers* here. It's just my personal experience, without anything that isn't already public.

## os-what?

## the course



### stuff i liked
- hands-on
- excellent mentoring support via discord.

### stuff i didn't \*love\*
- only 32bits
- ida pro but it's ida free, lol.

## reqs

You need to get confortable with:
- x86 arch
- asm 
- windows internals
- python scripting. you essentially need to be able to create a working exploit.


Thorought the course you will review IDA and Windbg, but they are *just tools*. The important part is to understand the concepts behind them.

## prep

Studied for 3 months.
- review references to get a taste of what I'll face.
- watched videos at 1.5x speed. I usually go 2x, but this stuff is *dense*.
- reviewed the book. thoroughly. taking notes. It turns out the book has a great intro to x86 arch and exploitation concepts. Sure, we can complement with other resouces like corelan.be, but the book itself is super comprehensive.
- did the 3 challenges. the first one specially is a good way to get humbled. learned a lot.
- reviewed my solutions and tried to optimize them and do it differently. Also make sure I understand ALL concepts involved behind every technique or command. 


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
- read rfc's. be comfortable reading protocol structure, format and fields.
- fuzz from smaller to larger payloads. Review with IDA the controls. For isntance: large inputs to get stackoverflow, smaller to pass filters.
- start from windbg and go to IDA. know when to switch between them.
- make sure IDA and windgb have the same offset to compare potatoes with potatoes. 
- calc the offset with rp++ with powerhell to get the library you want.
- when choosing modules to find gadgets, make sure they are not dynamic. Although they don't have ASLR! don't trust the !nmod from narly. 
- have script skeletons for common archetypes referenced in the book.
- rename the files with the attemps and the phase, not just numbers. You can always rollback or check what worked 3 versions ago. Don't do `poc_2_final_v3_asdasdasd_01.py`
- machines don't have av, so don't worry about encoding. The smaller the shellcode, the better. KISS.

## resources

- use offsec discord. linking your student account. they have an SLA of 15mins to answer your questions. And they are pretty good, IMO.
- the book
- corelan.be
- Buffer Overflows Made Easy TCM series. Basic stuff. But decent script-kiddie introduction.
https://www.youtube.com/watch?v=qSnPayW6F7U&list=PLLKT__MCUeix3O0DPbmuaRuR_4Hxo4m3G&index=1

## exam

#### timeline


## wh00t wh00t

