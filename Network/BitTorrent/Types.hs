{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}

module Network.BitTorrent.Types
  ( PeerId (..)
  , InfoHash (..)
  , Peer (..)
  , toBEBitField
  , bsToBEBitField
  , bsToNaturalBE
  , beBitFieldToNatural
  , beBitFieldToWord8
  , beBitFieldToBS
  )
where

import Control.Lens (_1, _3, view)
import Data.BEncode
import Data.Binary (Binary)
import qualified Data.Binary as Bin
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Put as Bin
import Data.Bits (FiniteBits (finiteBitSize), testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (foldl')
import Data.Monoid ((<>))
import Data.String (IsString)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word8)
import GHC.Generics (Generic)
import Network.Socket (HostAddress, PortNumber)
import Numeric.Natural (Natural)

--------------------------------------------------------------------------------
newtype PeerId = PeerId { getPeerId ∷ ByteString }
  deriving
    (Eq, Generic, IsString, Ord, Show)

instance Binary PeerId where
  get = PeerId <$> Bin.getByteString 20
  put = Bin.putByteString . getPeerId

newtype InfoHash = InfoHash { unInfoHash ∷ VU.Vector Bool } -- 160bit big endian
  deriving
    (Eq, Ord, Show)

instance Binary InfoHash where
  get = InfoHash . bsToBEBitField <$> Bin.getByteString 20
  put (InfoHash nId) = Bin.putByteString $ beBitFieldToBS nId

instance FromBEncode InfoHash where
  parseBEncode (String s) =
    either (fail . view _3) (pure . view _3) $ Bin.decodeOrFail $ LBS.fromStrict s
  parseBEncode _ = fail "InfoHash: expected String"

instance ToBEncode InfoHash where
  toBEncode = String . LBS.toStrict . Bin.encode

data Peer
  = Peer
      { peerAddress ∷ HostAddress
      , peerPort ∷ PortNumber
      , peerId ∷ Maybe PeerId
      }
  deriving
    (Generic, Show)

instance Eq Peer where
  p0 == p1 = peerAddress p0 == peerAddress p1
          && peerPort p0 == peerPort p1

instance Ord Peer where
  compare p0 p1 = compare (peerAddress p0) (peerAddress p1)
               <> compare (peerPort p0) (peerPort p1)

--------------------------------------------------------------------------------
-- | Computes the representation as a big endian bitfield.
toBEBitField ∷ FiniteBits a ⇒ a → VU.Vector Bool
toBEBitField a = VU.generate (finiteBitSize a) $ \i →
  testBit a (finiteBitSize a - 1 - i)

-- | Converts a 'ByteString' to a big endian bitfield.
bsToBEBitField ∷ ByteString → VU.Vector Bool
bsToBEBitField = VU.concat
               . map toBEBitField
               . BS.unpack 

-- | Converts a 'ByteString' to a Natural.
-- Considering the 'ByteString' as a big endian bitfield.
bsToNaturalBE ∷ ByteString → Natural
bsToNaturalBE = beBitFieldToNatural . bsToBEBitField

-- | Converts a big endian bitfield to a Natural.
beBitFieldToNatural ∷ VU.Vector Bool → Natural
beBitFieldToNatural v = foldl' (\n (i,b) → n + if b then 2^i else 0) 0
                      . zip [n-1,n-2 .. 0]
                      $ VU.toList v
  where
    n = fromIntegral $ VU.length v

-- | Converts a big endian bitfield to a 'Word8'.
beBitFieldToWord8 ∷ VU.Vector Bool → Word8
beBitFieldToWord8 v | VU.length v > 8 = error "beBitFieldToWord8: BitField too large"
                    | otherwise = fromIntegral $ beBitFieldToNatural v

-- | Converts a big endian bitfield to a 'ByteString'.
beBitFieldToBS ∷ VU.Vector Bool → ByteString
beBitFieldToBS = BS.pack . map beBitFieldToWord8 . chunk . normaliseLength
  where
    normaliseLength v = VU.replicate (VU.length v `rem` 8) False VU.++ v
    chunk v = [ VU.slice i 8 v | i ← [0,8 .. VU.length v - 8] ]
