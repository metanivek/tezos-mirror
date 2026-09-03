// SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

pragma solidity >=0.8.2 <0.9.0;

// Emits a log from a nested call frame.
contract LoggerB {
    event LogFromB(uint256 indexed value);

    uint256 public counter;

    function logValue(uint256 value) public {
        emit LogFromB(value);
    }

    // Takes up a call frame without emitting anything.
    function bump() public {
        counter += 1;
    }
}

// Emits a log, calls LoggerB (which emits its own log and returns), then emits
// another log. Used to check that the callTracer attributes each log to the
// call frame that actually emitted it.
contract LoggerA {
    event LogFromA(uint256 indexed value);

    LoggerB public b;

    constructor() {
        b = new LoggerB();
    }

    function run(uint256 first, uint256 second, uint256 third) public {
        emit LogFromA(first);
        b.logValue(second);
        emit LogFromA(third);
    }

    // Calls LoggerB twice, first silently, and emits a log after each: the
    // log after the silent call has a position no log order can derive.
    function runAfterSilentCall(
        uint256 first,
        uint256 second,
        uint256 third
    ) public {
        b.bump();
        emit LogFromA(first);
        b.logValue(second);
        emit LogFromA(third);
    }

    // Emits the very same log (same address, same topics, same data) at
    // two nested frames, once from an external self-call and once here.
    // [runDuplicateInnerFirst] and [runDuplicateOuterFirst] emit the same
    // logs in the same order, so their receipts are indistinguishable: only
    // the positions tell the two interleavings apart.
    function innerThenCall(uint256 value) public {
        emit LogFromA(value);
        b.logValue(value);
    }

    function callThenInner(uint256 value) public {
        b.logValue(value);
        emit LogFromA(value);
    }

    function runDuplicateInnerFirst(uint256 value) public {
        this.innerThenCall(value);
        emit LogFromA(value);
    }

    function runDuplicateOuterFirst(uint256 value) public {
        emit LogFromA(value);
        this.callThenInner(value);
    }
}
