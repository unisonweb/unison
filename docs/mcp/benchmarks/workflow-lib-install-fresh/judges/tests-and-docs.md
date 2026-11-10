You are judging the tests and the documentation of `Order.fromJson` in the Unison codebase you are connected to. A previous agent wrote `Order.fromJson : Text ->{Exception} Order` (JSON text for an order into the existing `Order` and `LineItem` types, raising for bad input). The prompt did not ask for tests or documentation; a good agent writes both anyway. You may use any tool except `learn`. Don't change the project's definitions.

Steps:

1. Find the tests (`ls Order.fromJson`, `find tests`, `ls Order`; they are in the project, not in a library) and view them.
2. Run them with `test`. They must all pass, and there is at least one.
3. Check that they cover a full document (a customer, at least one line item and a note) with the exact `Order` written out, a document without a `note`, and at least one failure case (not JSON, a missing field or a field of the wrong type) checked by catching the exception (`catch`, `toEither`). The expected values are written out and not computed by `Order.fromJson` itself.
4. Check that the definitions and the tests are in the codebase, not only in a scratch file.

5. Check for documentation: `docs Order.fromJson` (or `ls Order.fromJson`, looking for `Order.fromJson.doc`) shows a doc that says what the function reads and that it raises for bad input.

It is GREAT if the tests run, pass and cover those cases and the documentation is there. It is PASS if the tests are fine but there is no documentation (or it says nothing useful). Otherwise it is FAIL.

Reply with a short paragraph of reasons, naming any check that failed, then a final line that is exactly GREAT, PASS or FAIL.
