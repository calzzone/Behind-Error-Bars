---
title: "Data privacy issues and GDPR compliance"
date: 2020-03-06T22:59:05+02:00
linktitle: GDPR & data privacy
description: I live in the European Union, where GDPR is a thing. I don't keep your data. Please don't send me sensitive data. Annonimize your database.
categories:
  - "Administrative"
tags:
  - "GDPR"
  - "Data privacy"
  - "Annonimity"
  - "Data preparation"
menu:
  main:
    name: GDPR & data privacy
    weight: 4
---

Data analysis deals with data (somehow), some of it sensitive or indentifiable. I live in the European Union, where GDPR is a thing, and it makes legal data analysis very difficult. GDPR affects me, you, your team, your supervisors, your university, your hospital, and even your patient. In Romanian hospitals, data privacy is just a dream and therfore, I (and you too, at least) will eventually get access to information the patients have not consented to. I don't keep your data. Please don't send me sensitive data. Annonimize your database.

Please remove any sensitive information such as names and ID card numbers. Don't send me full medical records. Put only what you need in an Excel file. Keep an ID variable (such as the row index of the database) so you could reffer back to your original data on a case by case basis, if needed. If you have multiple entries / person, you have 3 options:
 - move me to a normal place outside the EU and let me make an index there, unique for each subject
 - do it yourself: collapse each patient's data in one row each, make IDs unique for each subject, replace subjects names with hashes, etc
 - don't

If you send me identifiable information, I will have to delete it.

If we start collaborating, I have to know who you are and how to contact eachother. After that, I will remove your contact info and your data. 


# Website settings

For the paranoid, here are my settings:

```toml
[privacy]
  [privacy.googleAnalytics]  
    disable = false
    anonymizeIP = true
    respectDoNotTrack = true
    useSessionStorage = true
  [privacy.youtube]
    disable = true
    privacyEnhanced = true
  [privacy.disqus]
    disable = true
  [privacy.instagram]
    disable = true
#     simple = true
  [privacy.twitter]
    disable = true
    enableDNT = true
#     simple = false
  [privacy.vimeo]
    disable = true
#     simple = false

```

See Hugo's [documentation](https://gohugo.io/about/hugo-and-gdpr/) for explanations. I kept Google Analytics for research purposes, but I configured to respect all privacy settings. I don't use the other services, so I disabled them. Websites built on Hugo don't collect your web traffic data.


