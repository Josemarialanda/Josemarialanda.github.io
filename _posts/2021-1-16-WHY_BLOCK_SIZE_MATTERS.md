---
layout: post
title: "Why Block Size Matters"
author: "José María Landa Chávez"
categories: Blockchain, cryptocurrency
tags: [blockchain, cryptocurrency, bitcoin]
image: WHY_BLOCK_SIZE_MATTERS.jpg
---

Bitcoin itself is already quite contraversial topic. But even within the Bitcoin community there is fiery debate as to what the size of a block should be.

Block size is such a debated topic that it caused a split in the Bitcoin community some years back. Why does the size of a block even matter? TPS, or transactions per second. The quation of scale in bitcoin is not a new one. But as transaction volumes are expected to increase in the years ahead, users begin to worry their precious technology won't be able to keep up with newer cryptocurrencies. 

## What are Blocks

Blocks are bundles of transactions which are confirmed and subsequently shared on the blockchain. In the early days of Bitcoin, Blocks had a storage capacity of precisely 36MB. However, this was changed in 2010 when the core Bitcoin developers made the decision to reduce block size to 36MB to reduce the threat of spam and potential [denial-of-service attacks](https://www.paloaltonetworks.com/cyberpedia/what-is-a-denial-of-service-attack-dos#:~:text=A%20Denial%2Dof%2DService%20(,information%20that%20triggers%20a%20crash.) on the network. 

In present year, 2021, the amount of data in each block is increasing, some blocks are even hitting this maximum of 1MB. This leads to capacity issues on the network and delayed confirmations which in turn leads to a low TPS coefficient. 

![](/assets/img/WHY_BLOCK_SIZE_MATTERS/A.png)

**Bitcoin transactions per second. Source: Blockchain.com**

##  Why Does Block Size Matter?

The size of a block has a direct impact on the number of transactions per second TPS the bitcoin network is capable of processing and can thus be seen to inhibit the network’s ability to scale. When blocks are full to the brim, the network becomes congested causing transaction fees to rise atronomically, making the currency ineffienct for day to day use. 

Over the years, Bitcoin has seen numerous proposals for improvement concerning block size, in the form of BIP's, short for Bitcoin Improvement Proposals. Many argue that an increase is needed in order to reduce fees, process more transactions per second and allow Bitcoin to scale to compete with mainstream payments technologies.  

On May 4, 2015, Gavin Andresen published an article titled *“Why increasing the max block size is urgent,”* Andresen warned:

> If the number of transactions waiting gets large enough, the end result will be an over-saturated network, busy doing nothing productive. I don’t think that is likely — it is more likely people just stop using Bitcoin because transaction confirmation becomes increasingly unreliable.”

Despite numerous efforts on his behalf, nothing was done to increase the block size and the 1MB limit continued in full effect. 

## BIP101

BIP101 was a Bitcoin Improvement Proposal that proposed the maximum block size be raised to 8 MB as of Jan. 11, 2016, before increasing linearly to double every 730 days until January 2036. This proposal was well received bu the public. The 8 MB limit was estimated to be able to facilitate the processing of 24 transactions per second. However, consensus wasn't reached; the Bitcoin community remained divided on the issue of block size. Bram Cohen, the creator of the famous torrent client Bittorrent, argued in favor of transactions fees being determined by market forces amid the maintenance of the 1 MB block limit:

> “The proposed ‘solution’ to the ‘problem’ of hitting the transaction rate limit is to raise the limit from 1 megabyte to 20 megabytes. This sort of change flies directly in the face of the ethos of Bitcoin.”

Cohen argued that the prevalence of high fees would show that Bitcoin "provides real value" and stressed the incentive that miners would be given to protect the network in return for such an option. Cohen also added:

> “In the long term the mining rewards for Bitcoin will go away completely (there’s a strict schedule for this) and all that’s left will be transaction fees. Attempting to ‘solve’ the problem of transaction fees would in the long run undermine the security of Bitcoin even if it were done perfectly.”

## Some Bitcoin Forks Caused by Block Size changes...

* Bitcoin XT

* Bitcoin Unlimited

* Bitcoin Classic

* Segwit2x

**Not one of these forks has succeeded in forcing a block size increase.**

## Arguments in favor of increasing the blocksize

* Higher TPS coefficient

* Lower fees

* Fewer congestions

## Arguments in opposition to increasing the blocksize

* A low blocksize limit encourages higher transactions fees to incentivize miners.

* Risk of catastrophic consensus failure.

* Orphan rate amplification, more reorgs and double-spends due to slower propagation speeds.

* No amount of max block size would support all the world's future transactions on the main blockchain (various types of off-chain transactions are the only long-term solution)

* A hard fork requires waiting for sufficient consensus.

Furthermore, an increase in the block size can cause catastrophic damage to decentralization. Larger blocks make full nodes more expensive to operate, which in turn leads to less users running full nodes, which leads to centralized entities having more power, which makes Bitcoin require more trust, which weakens Bitcoins value proposition. A  deceivingly measly increase to 8MB could easly give chinese pools a huge advantage against european miners. Electricity in China is generally much cheaper than in Europe. Thus, decentralizing the network in favor of a larger block size is not a viable compromise; Bitcoin is only useful if it is decentralized because centralization requires trust. Bitcoins value proposition is trustlessness. 

## A History of Bitcoin Improvement Proposals

* **BIP 100**: Change block size limit based on miner votes, but don't leave the range (1MB, 32MB) without a softfork or hardfork respectively.

* **BIP 101**: BIP101 was a Bitcoin Improvement Proposal that proposed the maximum block size be raised to 8 MB as of Jan. 11, 2016

* **BIP 102**: Increase to 2 MB on November 11, 2015.

* **BIP 103**: Increase by 17.7% annually until 2063.

What do you think? Should the 1MB limit be changed? Will Bitcoin survive the new era of advanced cryptocurrencies?