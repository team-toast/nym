from pprint import pprint
import hashlib
import json

def loadTargetAddresses():
    with open('aidrop-targets','r') as f:
        data = f.read()
        return data.split('\n')

def hashhex(hexData):
    b = bytes.fromhex(hexData)
    return hashlib.sha256(b).hexdigest()

def charIsDec(char):
    try:
        int(char)
        return True
    except:
        return False

def main():
    salt = input("input salt: ")
    result = genWithSalt(bytes.fromhex(salt))
    with open('airdrop_pairs.json','w') as f:
        json.dump(result, f)
    print("Written to airdrop_pairs.json")

def genWithSalt(salt):
    # load addresses
    addresses = loadTargetAddresses()
    # order alphabetically
    addresses.sort()

    claimPairs = []
    for address in addresses:
        toHash = salt.hex() + address[2:]
        nymId = hashhex(toHash)
        claimPairs.append((address, nymId))
    
    print("Generated info for",len(claimPairs),"addresses.")
    print("Checking for problematic pairs.")
    
    for pair in claimPairs:
        foundNonDecChar = False
        for char in pair[1]:
            if not charIsDec(char):
                foundNonDecChar = True
                break
        if not foundNonDecChar:
            print("found a problematic pair!", pair)

    return claimPairs

if __name__ == "__main__":
    main()