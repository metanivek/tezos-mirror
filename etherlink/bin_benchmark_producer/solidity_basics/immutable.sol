// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

contract Immutable {
    // coding convention to uppercase constant variables
    address public immutable MY_ADDRESS;
    uint public immutable MY_UINT;

    constructor(uint _myUint) {
        MY_ADDRESS = msg.sender;
        MY_UINT = _myUint;
    }

    // Explicit getter for MY_ADDRESS
    function getMyAddress() public view returns (address) {
        return MY_ADDRESS;
    }

    // Explicit getter for MY_UINT
    function getMyUint() public view returns (uint) {
        return MY_UINT;
    }
}