pragma solidity ^0.8.0;

import "@openzeppelin/contracts/token/ERC1155/ERC1155.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract AlphaNymAirdropSet is ERC1155, Ownable 
{
    bool public mintingClosed = false;
    
    // OpenSea's shitty nonstandard ERC1155 support requires this to have a name displayed for the set.
    string public constant name = "Alpha Nyms";

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

contract Batcher
{
    function _mint(AlphaNymAirdropSet airdropContract, address who, uint nymId)
        internal
    {
        bytes memory data = abi.encodeWithSignature("mint(address,uint256)", who, nymId);
        address(airdropContract).call(data);
    }
}

contract dropBatch1 is Batcher {
    constructor(AlphaNymAirdropSet airdropContract) {

        // fill me with 500 lines from an airdrop_cmds.txt

    }
}
