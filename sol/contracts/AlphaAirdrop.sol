pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC1155/ERC1155.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract AlphaNymAirdropSet is ERC1155, Ownable 
{
    bool public mintingClosed = false;
    
    // OpenSea's shitty nonstandard ERC1155 support requires this to have a name displayed for the set.
    string public constant name = "Alpha Nyms Batch Drop Test";

    constructor() 
        ERC1155("https://etc.foundrydao.com/nym_metadata.php?id={id}") 
        Ownable()
    { }
    
    function mint(address who, uint nymId)
        external
    {
        require(tx.origin == owner(), "I only listen to my daddy!");
        require(!mintingClosed, "Minting has been closed");

        _mint(who, nymId, 1, "");
    }
    
    function closeMinting()
        external
        onlyOwner
    {
        mintingClosed = true;
    }

    function updateUri(string memory _uri)
        external
        onlyOwner
    {
        _setURI(_uri);
    }
}

contract dropTest {
    constructor(AlphaNymAirdropSet airdropContract) {

airdropContract.mint(0x0000000000000000000000000000000000000001, uint256(0x5d7e15ae1b6a6c71a52f6f3257344005bd15ee8bae91aedb67fa7db6946db66b));
airdropContract.mint(0x0000000000000000000000000000000000000001, uint256(0xb05c968cc2fee4ca4cd3d2ca6e0d9c1d976dcdfb37652097d4aac78dad68367f));
airdropContract.mint(0x0000000000000000000000000000000000000001, uint256(0x2272293d19a422f90474fbf2a28f0961208c7683081c77715f0b12ac3555ccfd));
airdropContract.mint(0x0000000000000000000000000000000000000001, uint256(0xbdcacfe00c5ce4b1e715c15d239e1126ac48069280f9fe41f30338627c132ddf));
airdropContract.mint(0x0000000000000000000000000000000000000001, uint256(0x77ffadf549329f88b0fbdb3e70d5a83e6d74225fe17060738ef1d2c4070b9d2f));


    }
}
