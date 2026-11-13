# Revision history for network-control

## 0.1.7

* Implementing setLRUCapacity.

## 0.1.6

* Allowing size 0.

## 0.1.5

* New API: `lookup'` adjusts the target priority.
* New API: `LRUCacheRef` stuffs
* `insert` rebuilds PSQ when reached the limit.

## 0.1.4

* Using Integer instead of Int in LRUCache.

## 0.1.3

* Simplify `maybeOpenRxWindow` and improve docs
  [#7](https://github.com/kazu-yamamoto/network-control/pull/7)

## 0.1.2

* introducing a minimum size for window update
  [#5](https://github.com/kazu-yamamoto/network-control/pull/5)

## 0.1.1

* Change defaultMaxData
  [#4](https://github.com/kazu-yamamoto/network-control/pull/4)

## 0.1.0

* Breaking change: Renaming rxfWindow to rxfBufSize.
* Updating the document about flow control.

## 0.0.2

* Adding constants.

## 0.0.1

* Supporting GHC 8.10.

## 0.0.0

* First version.
