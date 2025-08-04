---
title: "Alpine"
date: 2024-02-12
draft: false
image: '/img/sochi.jpg'
type: "sport-section"
sport: "alpine"
---

Debug info:
- Image param: {{ .Params.image }}
- Relative URL: {{ .Params.image | relURL }}
- Default image: {{ .Site.Params.header_image }}