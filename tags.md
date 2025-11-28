---
layout: page
title: Tags
permalink: /tags.html
---

{% assign tags = site.tags | sort %}

{% for tag in tags %}
  <h3 id="{{ tag[0] | slugify }}">#{{ tag[0] }}</h3>
  <ul>
    {% for post in tag[1] %}
      <li>
        <a href="{{ post.url | relative_url }}">{{ post.title }}</a>
        <small>{{ post.date | date: site.theme_config.date_format }}</small>
      </li>
    {% endfor %}
  </ul>
{% endfor %}
