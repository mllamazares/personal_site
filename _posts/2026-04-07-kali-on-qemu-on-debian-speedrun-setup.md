---
layout: post
slug: kali-on-qemu-on-debian-speedrun-setup
title: "kali on qemu on debian: a speedrun setup"
tags: [tutorial, virtualization]
---

After years of virtualbox and vmware, I finally moved to qemu. I run debian as my daily driver, and qemu/kvm is *native* to the kernel, so you can forget about the third-party kernel modules that break on every update, the _"please reinstall guest additions"_ rituals. 🤮

As you might have guessed, the main thing I virtualize is kali. Qemu with kvm acceleration gives you near-native performance because it hooks directly into the linux kernel's virtualization extensions (intel vt-x / amd-v) instead of going through a compatibility layer like virtualbox does[^1].

You need to tweak a few things, so here is the light version of the process I followed.

### step #1: download the kali qemu image

Go to the [kali vm downloads site](https://www.kali.org/get-kali/#kali-virtual-machines) and grab the qemu image. It comes as a `.7z` archive containing a `.qcow2` disk image, which is the native format for qemu.

Extract it:

```bash
7z x kali-linux-*-qemu-amd64.7z
```

You'll get a `.qcow2` file. That's your virtual disk. No iso, no installer, no 45-minute setup wizard. Just a ready-to-boot image[^2]. 🫦

### step #2: install gnome boxes

```bash
sudo apt update
sudo apt install gnome-boxes qemu-system-x86 -y
```

Gnome Boxes is the frontend. It's clean, minimal, and does 90% of what you need. Think of it as the _"it just werks"_ layer on top of qemu/kvm.

### step #3: install virt-manager (you'll need it)

```bash
sudo apt install virt-manager
```

Gnome Boxes is great for day-to-day use, but it hides a lot of knobs. Virt-manager exposes the full configuration: cpu topology, disk bus types, network modes, firmware selection, etc. You'll want it for the initial setup.

### step #4: configure virt-manager

Launch it:

```bash
/usr/bin/python3 /usr/bin/virt-manager
```

Then go to **Edit** > **Preferences** > **New VM** and set **Graphics Type** to **VNC**.

This is the key step. Vnc graphics work cleanly with Gnome Boxes out of the gate, no guest agent installation required. You skip the whole `spice-vdagent` dance that every other guide tells you to do.

### step #5: import the image in gnome boxes

Click "Add" in Gnome Boxes. Point it to the `.qcow2` file you extracted. It will create a vm from it.

Boot it up. Default creds are `kali:kali`[^3].

### step #6: set the resolution

Go to kali's display settings and change the resolution to whatever your monitor supports. With vnc graphics, the resolution list should be available right away. 

If it looks weird, reboot the vm. Rebooting fixes like 80% of display issues in virtualization, and that percentage has been stable since roughly 2004, kek.

### bottom line

That's pretty much it. Maybe 10 min if you count the download. Now you've got a kali vm running on a native hypervisor with working display scaling and no guest agent fiddling. 

Now let's break some stuff, shall we? 


[^1]: virtualbox technically *can* use kvm as a backend since version 6.1, but the integration is experimental and honestly feels like it was added so Oracle could tick a checkbox somewhere, kek.
[^2]: kali maintains pre-built images for qemu, vmware, virtualbox, and hyper-v. The qemu one uses `qcow2` format, which supports thin provisioning (the file grows as you use it instead of pre-allocating the full disk size). Nice if you're not swimming in storage.
[^3]: if you didn't already know that reconsider your career choices.
