# The Clash UDBC-Arbiter Verification Challenge

The repository offers arbiter an implementation in [Clash](https://clash-lang.org) for a bounded number of clients and arbitrary (theoretically unbounded) data packages to be exchanged. Data packages additionally are classified according to LOW or HIGH security levels distributed along two output channels with respective access control, i.e., the LOW output channel only has access to LOW security data, while the HIGH security output channel has access to all data that is returned by the arbiter.

## What's the challenge?

The challenge is to formally verify that the given implementation satisfies the following safety and security properties for any number of clients and all potential data types:

* __Mutual Exclusion:__ _No more than one request is granted at a time._
* __Productivity:__ _Every open request eventually gets granted._
* __Fairness:__ _There exists an upper bound on the number of cycles, after which every request gets granted._
* __Access Control:__ _Sensitive data never appears on the LOW output channel._
* __Non-interference:__ _HIGH inputs cannot observably influence LOW outputs._
* __Data Integrity:__ _The data attached to requests is the same as the data delivered with the corresponding grants._

Note that clients are allowed to place new requests although their previous requests have not been granted yet, in which case the previous requests are automatically closed and replaced by the newest one. The above properties only consider open requests in that sense.

## Getting Started

Load the example with the Clash interpreter:

```
clashi Arbiter.hs
```

Simulate the arbiter on some inputs:

```
clashi> simulateArbiter
```
