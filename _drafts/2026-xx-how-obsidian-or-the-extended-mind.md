

Extended mind
https://grokipedia.com/page/Extended_mind_thesis



### why

file over app.
privacy.
customizable.
extensible.
performance.

### plugins

- git sync
- obsidian web clipper
- flexoki theme
- bases
- omnisearch 
- autolink title
- editing toolbar


#### note on syncing in iOS.

I'm a weird guy that for some reason uses debian as daily driver but it has folded onto the system and also uses an iphone. The thing is that Obsidian sync in iOS is supported by a 3rd party app. And iOS deliberately restricts background processing for third-party apps to preserve battery life. When you switch away from Obsidian or lock your screen, the operating system freezes the app entirely.  

AFAIK, this affects to all non official obsidian sync methods. The one that I use is GitSync. And they promise something like "You can setup scheduled sync, which will sync as often as iOS allows (can be as often as 15 minutes after some time)". The bottleneck is not your Git setup or the specific plugin you choose. The bottleneck is Apple.

Follow this guy's tutorial: https://viscouspotenti.al/posts/gitsync-all-devices-tutorial

TL;DR, if you use iOS:
- pay Obsidian Sync
- use GitSync and sync manually.
- don't sync iOS.

### how to structure the notes

First lesson, f*ck folders. Yes. You hear it right: fuck. folders.

CEO of Obsidian uses this:
--

https://www.youtube.com/watch?v=Dq3R3uS0sQ4
https://stephango.com/vault 
https://www.reddit.com/r/Zettelkasten/top/?t=all


> Write to the nth power, the n - 1 power, write with slogans: Make rhizomes, not roots, never plant! Don't sow, grow offshoots! Don't be one or multiple, be multiplicities! Run lines, never plot a point! Speed turns the point into a line! Be quick, even when standing still! Line of chance, line of hips, line of flight. Don't bring out the General in you! Don't have just ideas, just have an idea (Godard). Have short-term ideas. Make maps, not photos or drawings. Be the Pink Panther and your loves will be like the wasp and the orchid, the cat and the baboon. 

Fun fact: I studied philosophy while working while freelancing. 

I guess if you are still reading this you are really interested.

### how to llm your notes

https://medium.com/@martk/obsidian-claude-code-the-claude-code-skills-that-make-your-vault-feel-alive-4ec05a1ec1e6






## Debian.

1. Create a PAT in Github here: [Sign in to GitHub · GitHub](https://github.com/settings/tokens)
2. Install `obsidian-git` community plugin inside the vault.  
    `Settings > Community Plugins > Search`
3. Create a github account or skip if you have one.
4. Create new `repository` **with README** . _(ex. you/notes)_
5. Create a fine-granted access token to access the repo. [here](https://github.com/settings/tokens?type=beta)
6. Change to vault directory and clone using the access token.  
    `git clone https://<token>@github.com/you/notes.git`
7. Copy the vault folder to your mobile device.
8. Finally restart Obsidian and you are good to go!

[The Easiest Way to Setup Obsidian Git (to backup notes) - Share & showcase - Obsidian Forum](https://forum.obsidian.md/t/the-easiest-way-to-setup-obsidian-git-to-backup-notes/51429/6)

## iPhone

Install the 

[How to Sync Obsidian Across All Devices Using Git, Automatically and for Free](https://viscouspotenti.al/posts/gitsync-all-devices-tutorial)