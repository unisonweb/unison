# ChangeLog for http-semantics

## 0.3.0

* Breaking change: fillFileBodyGetNext takes Sentinel instead of
  IO () to close files on time.

## 0.2.1

* Add outBodyCancel to OutBodyIface
  [#11](https://github.com/kazu-yamamoto/http-semantics/pull/11)
* Documentation improvement.
  [#10](https://github.com/kazu-yamamoto/http-semantics/pull/10)
  [#11](https://github.com/kazu-yamamoto/http-semantics/pull/11)

## 0.2.0

* Introduce `responseStreamingIface`
  [#9](https://github.com/kazu-yamamoto/http-semantics/pull/9)

## 0.1.2

* Avoid buffer overflow in fillBufBuilderOne
  [#4](https://github.com/kazu-yamamoto/http-semantics/pull/4)

## 0.1.1

* Avoid buffer overflow in runStreamingBuilder
  [#3](https://github.com/kazu-yamamoto/http-semantics/pull/3)

## 0.1.0

* Make it possible to guarantee that final DATA frame is marked end-of-stream.
  [#2](https://github.com/kazu-yamamoto/http-semantics/pull/2)

## 0.0.1

* Defining getResponseBodyChunk'.
  [#1](https://github.com/kazu-yamamoto/http-semantics/pull/1)

## 0.0.0

* The first release.
