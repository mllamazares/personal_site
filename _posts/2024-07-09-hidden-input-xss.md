---
layout: post
slug: hidden-input-xss
title: triggering XSS in hidden inputs
tags: [bugbounty, xss]
---

Did you know you can still exploit XSS in a hidden input? (I didn't either).

I found an injection point within a hidden input type. This is difficult to exploit because typical JS events like `onmouseover` and `onfocus` don't work in this scenario. 

While researching, I stumbled upon [a great article from PortSwigger](https://portswigger.net/research/xss-in-hidden-input-fields) by Gareth Heyes that uses the `accesskey` attribute (activated by `Alt+Shift+[Key]`) to trigger the `onclick` event on the hidden input.

In this case, I ended up using the following payload, which also displays `Press Alt+Shift+X to continue` to maximize the chances of success 😈:

```
https://REDACTED/subscribe?source=mllamazares%22%20accesskey=%22X%22%20onclick=%22alert(document.domain)%22/%3EPress%20Alt%2BShift%2BX%20to%20continue%20
```

![hidden input xss](/assets/img/hidden-input-xss1.png)

Although this method involves user interaction, requires less social engineering than an average self-XSS. I'm sharing this for the creativity of the attack, not for its potential impact (usually triaged as low or informative).

**bonus**: here's [another cool method](https://x.com/garethheyes/status/1854191120277733760) [^1] to do this which does _\*not\*_ require user interaction [^2] that leverages `oncontentvisibilityautostatechange` with `content-visibility: auto` to trigger the `alert` event:

```
https://REDACTED/subscribe?source=mllamazares%22%20oncontentvisibilityautostatechange%3Dalert%281%29%20style%3Dcontent-visibility%3Aauto%3E%20
```

<br>

[^1]: also by Gareth Heyes
[^2]: just works in chrome