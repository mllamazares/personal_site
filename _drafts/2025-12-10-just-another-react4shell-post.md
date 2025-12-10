---
layout: post
slug: passing-oswe
title: JAR4SP (Just Another React4Shell Post)
tags: [rce, react, react4shell, cve]
---

As everyone is speaking about this, I guess this is JAR4SP (Just Another React4Shell Post).

I wanted to create this post because most takes I've seen don't actually explain the exploit from a white-box perspective.

### react4-what?

 CVE-2025-55182 aka React4Shell, is one of the most critical vulns discovered in December 2025, with a maximum CVSS score of 10.0. It bassically allows unauthenticated remote code execution through a single crafted HTTP request.
 

### context

This vulnerability affects React Server Components (RSC) and the frameworks that implement them, particularly Next.js. 

React Server Components is a feature introduced in React 19 that allows components to be rendered on the server rather than the client’s browser. This provides significant performance benefits, as the server can handle computationally intensive tasks whilst sending only the rendered output to the client.

The communication between the server and client in RSC relies on a protocol called React Flight. This protocol handles the serialisation and deserialisation of data being transmitted between the server and client.

```js
const files = {
  "0": '["$1"]',
  "1": '{"object":"fruit","name":"$2:fruitName"}',
  "2": '{"fruitName":"cherry"}',
};
```

When React decodes this:

$1 → chunk 1
$2:fruitName → chunk 2's fruitName property
Reconstructed result:

```js
[{ object: "fruit", name: "cherry" }]
```

server side rendering is a node server that processes your code, figures out the final html, then sends that to the user

client side rendering is that process being done on the user's browser instead

react server components combine the two by having the node server figure out which parts can be ssr'd and which parts can be csr'd, then delivers the ssr'd html as well as the necessary JavaScript to build the csr'd portion

### let's see the code

It is fundamentally an unsafe deserialization vulnerability in how React Server Components handle incoming Flight protocol payloads. The vulnerability exists in the requireModule function within the react-server-dom-webpack package. Let’s examine the problematic code pattern:

```js
function requireModule(metadata) {  
 var moduleExports = __webpack_require__(metadata[0]);  
 // ... additional logic ...  
 return moduleExports[metadata[2]];  // VULNERABLE LINE  
}
``` 

The critical flaw is in the bracket notation access moduleExports[metadata[2]]. In JavaScript, when we access a property using bracket notation, the engine doesn’t just check the object’s own properties—it traverses the entire prototype chain. This means an attacker can reference properties that weren’t explicitly exported by the module.

Most importantly, every JavaScript function has a constructor property that points to the Function constructor. By accessing someFunction.constructor, an attacker obtains a reference to the global Function constructor, which can execute arbitrary JavaScript code when invoked with a string argument.

The vulnerability becomes exploitable because React’s Flight protocol allows clients to specify these property paths through the colon-separated reference syntax. An attacker can craft a reference like $1:constructor:constructor which traverses:

Get chunk/module 1
Access its .constructor property (gets the Function constructor)
Access .constructor again (still the Function constructor, but confirms the chain).

### exploit


### how to test

Please, do not ever use a online instance that allows you to test this vuln. Just test it by your own means. This is because you basically don't have visibility of what the author is going to do with that info. They could be creating a DB of vulnerable servers ([it happens](/dont-blindly-trust-public-exploits)). 

The proper way to test this vuln is just to keep calm and use nuclei:

```bash
nuclei -t cves/2025/CVE-2025-55182.yaml -t yourwebsite.com
```

This template has been created by Assetnote the verification is well designed to avoid FPs. 

### testing in a lab 

If you just want to test it, I recommend two options:
- [TryHackMe - React2Shell: CVE-2025-55182](https://tryhackme.com/room/react2shellcve202555182) (1337 friendly)
- [vulhub/react/CVE-2025-55182 at master · vulhub/vulhub](https://github.com/vulhub/vulhub/tree/master/react/CVE-2025-55182)

### waf bypass 

As this vuln has caused quite a stir and basically is not feasible to patch the whole internet in a day, that's when WAF entered the room and started blocking most of the naive attempts. 

However, as we will see, WAFs are not a silver bullet and should not be used as a sole mitigation strategy. 

### original assetnote bypass

```yaml

``` 

### the vercel beef

Shubs, the CEO of Assetnote, tweeted that several WAF are vulnerable:

{% include twitter.html id="1996729020428538337" %}

The Vercel CEO told that the bypass they posted was just for Cloudflare. Then Shubs posted a bypass specific for Vercel:
{% include twitter.html id="1997063075422429657" %}

Then Vercel opened an bug bounty program that paid 50K for a bypass.
In less that 24h it drained 750K. Mostly by the Assetnote team that got some bypasses.

{% include twitter.html id="1998072892391592195" %}

That was insane. I endorse Vercel's posture.

### some bypasses

Of course, although I came late to the party, I gave it a try to bypass Vercel's WAF. Spoiler: I failed.

First off, I tried to use a simple bypass with a junk payload:



Finally, there are some bypasses that worked with different WAFs:

#### junk bypass



#### unicode bypass 



#### own bypass 


### bottom line

