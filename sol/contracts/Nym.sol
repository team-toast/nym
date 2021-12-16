pragma solidity ^0.8.0;

import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/utils/math/SafeMath.sol";

// NOTE: These are not final. More brainstormy. We are currently only doing airdrops, so don't use the below code yet.

struct OpenNymBid {
    uint nymData;
    uint currentBidAmount;
    address payable currentBidHolder;
    uint bidCloseTime;
}

contract NymDataCycler {
    // helps guarantee that we'll never accidentally generate the same Nym twice.
    uint private latestNymData;

    function getNextNymData()
        internal
        returns (uint)
        {
            iterateNymData();
            return latestNymData;
        }

    constructor() {
        setInitialPRandUint();
    }

    function setInitialPRandUint()
        private
        {
            latestNymData = uint(blockhash(block.number - 1));
        }

    function iterateNymData()
        private
        returns (uint)
        {
            latestNymData = uint(keccak256(abi.encode(latestNymData, blockhash(block.number-1))));
            return latestNymData;
        }
}

contract Nyms is Ownable, ERC721, NymDataCycler {
    using SafeMath for uint;

    uint constant ONE_PERC = 10**16; // 1.0%
    uint constant HUNDRED_PERC = 10**18; // 100.0%

    uint8 constant public NUM_BIDDING_NYMS = 9;
    uint constant public BID_FINALIZE_TIME = 1 days;
    uint constant public MIN_OUTBID_MULTIPLIER_PERC = 200 * ONE_PERC;

    OpenNymBid[NUM_BIDDING_NYMS] offeredNyms;
    uint private latestNymData;
    
    string public baseURI; // used for NFT standard

    address payable public incomeTarget;

    address public otherRandomMintCaller;

    constructor(address payable _incomeTarget, string memory _initialBaseURI)
        ERC721("Nym", "NYM")
        {
            incomeTarget = _incomeTarget;
            baseURI = _initialBaseURI;
            otherRandomMintCaller = address(0x0);

            for (uint8 i=0; i < NUM_BIDDING_NYMS; i++) {
                offeredNyms[i] = newNym();
            }
        }
    
    function changeOtherRandomMintCaller(address _addr)
        external
        onlyOwner()
        {
            otherRandomMintCaller = _addr;
        }

    function changeBaseURI(string memory _newBaseURI)
        external
        onlyOwner()
        {
            baseURI = _newBaseURI;
        }
    
    function placeBid(uint8 bidId, uint expectedNymData, address payable bidder)
        public
        payable
        {
            OpenNymBid storage offeredNym = safeGetOpenBid(bidId);
            require(offeredNym.nymData == expectedNymData, "Didn't get expected nymData.");

            bool alreadyHasBid = (offeredNym.currentBidHolder != payable(0x0));

            uint minBidRequired;
            if (alreadyHasBid) {
                minBidRequired = offeredNym.currentBidAmount.mul(MIN_OUTBID_MULTIPLIER_PERC).div(HUNDRED_PERC);
            }
            else {
                minBidRequired = 1 wei;
            }
            
            require(msg.value >= minBidRequired, "You didn't include the minimum bid amount.");

            // try to refund previous bidder, ignore success
            bool success = offeredNym.currentBidHolder.send(offeredNym.currentBidAmount);
            // whether it fails or not, send all remaining funds to Gulper or revert
            require(incomeTarget.send(address(this).balance), "Failed to capture funds!");

            // Bid passes tests, let's make it happen!
            updateBid(bidId, msg.value, bidder);
        }
    
    function updateBid(uint8 bidId, uint _newBidAmount, address payable _newBidHolder)
        internal
        {
            offeredNyms[bidId].currentBidAmount = _newBidAmount;
            offeredNyms[bidId].currentBidHolder = _newBidHolder;
            offeredNyms[bidId].bidCloseTime = block.timestamp.add(BID_FINALIZE_TIME);
        }
    
    function poke()
        external
        {
            for (uint8 i=0; i<NUM_BIDDING_NYMS; i++) {
                mintOrCycle(i);
            }
        }
    
    function mintOrCycle(uint8 bidId)
        internal
        returns(bool success)
        {
            OpenNymBid storage offeredNym = safeGetOpenBid(bidId);

            if (block.timestamp < offeredNym.bidCloseTime) {
                return false;
            }

            if (offeredNym.currentBidHolder != payable(0x0)) {
                _safeMint(offeredNym.currentBidHolder, offeredNym.nymData);
            }

            offeredNyms[bidId] = newNym();

            return true;
        }
    
    function externalMint(uint _nymData, address _to)
        external
        {
            require(msg.sender == otherRandomMintCaller);
            _safeMint(_to, _nymData);
        }
    
    function newNym()
        internal
        returns (OpenNymBid memory)
        {
            return OpenNymBid(
                getNextNymData(),
                0,
                payable(0x0),
                block.timestamp.add(BID_FINALIZE_TIME)
            );
        }
    
    function safeGetOpenBid(uint8 bidId)
        internal
        view
        returns(OpenNymBid storage)
        {
            require(bidId < NUM_BIDDING_NYMS, "Bid ID invalid (out of array bounds).");
            return offeredNyms[bidId];
        }
    
    function _baseURI()
        override
        internal
        view
        returns (string memory)
        {
            return baseURI;
        }
}