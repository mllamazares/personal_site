---
layout: post
slug: passing-osed
title: my experience passing the osed cert
tags: [cert, pentesting]
---

As you might know, I'm prepping for the OSED cert. So, I'm a huge fan of the Feynman method and I going to try to explain my thought process because is a nice way to build up my understanding of the topic. 

For this, I will not use a particular binary, but explain the concepts in the context of x86 arch and using windbg.

note: i'm not an expert in binary exploitation, so feel free to insult me if you have any feedback.

note: this tutorial assumes you already know how to exploit a vanilla BoF.

## you shall not execute! 

We can have different protections that will mess up with us, such as ASLR, SEH or DEP. Here we will focus in DEP.
DEP or Data Execution Prevention tell the program to NOT execute our code because that's not a executable memory section.
So our previous exploit will go back to the shellcode but will NOT be executed. \*play sad trumpet.mp3

To overcome this, we can perform a thing called ROP chain.

There are two strategies:
1. building the shellcode by hand. this is complex and a nightmare.
2. call an API to make our shellcode executable.

Here we will explore the second option.

### api calls 

To do this, we have different options sorted by my personal taste:
VirutalProtect
VirtualAlloc
WriteProcessMemory

For this example we will use VirtualProtect()

http://msdn.microsoft.com/en-us/library/aa366898(VS.85).aspx

The VirtualProtect function changes the access protection of memory in the calling process.

BOOL WINAPI VirtualProtect(
  __in   LPVOID lpAddress,
  __in   SIZE_T dwSize,
  __in   DWORD flNewProtect,
  __out  PDWORD lpflOldProtect
);
If you want to use this function, you will have to put 5 parameters on the stack :

Return address	pointer to the location where VirtualProtect() needs to return to. This will be the address of your shellcode on the stack (dynamically created value)
lpAddress	pointer to the base address of the region of pages whose access protection attributes need to be changed. In essence, this will be the base address of your shellcode on the stack (dynamically created value)
dwsize	number of bytes (dynamically created value, making sure the entire shellcode can get executed. If the shellcode will expand for some reason (because of decoding for example), then those additional bytes will need to be taken into account and accounted for.
flNewProtect	option that specifies the new protection option : 0x00000040 : PAGE_EXECUTE_READWRITE. If your shellcode will not modify itself (decoder for example), then a value of 0x00000020 (PAGE_EXECUTE_READ) might work as well
lpflOldProtect	pointer to variable that will receive the previous access protection value
Note : The memory protection constants that can be used in VirtualProtect() can be found here

On XP SP3, VirtualProtect() is located at 0x7C801AD4 (kernel32.dll)

### how to call it

First, let's see how a plain call to virtual alloc would look like.




### sniff sniff, gadgets, where are you?

To find gadgets the easiest way is to use mona.
