module SocketsAndPipes.Syllabus
  (
    -- * Chapters
    -- $chapters

    -- * Libraries
    -- $libraries
  )
  where

import Prelude ()

import qualified ASCII as A
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans.Resource as Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Time as Time
import qualified Network.Simple.TCP as Net
import qualified Prelude

{- $chapters

List of modules that make a significant appearance in each chapter:

__Setup__

  * "Relude" - Alternate prelude that includes a lot of standard utilities
  * "System.Directory", "System.FilePath" - Determining where our data files should go

__1 - Handles__

  * "System.IO" - Writing to a file
  * "Control.Monad.Trans.Resource" - Using 'Resource.allocate' to ensure that file handles are closed

__2 - Chunks__

  * "Data.Text" - 'T.Text' is a chunk of characters
  * "Data.Text.IO" - Reading and writing files using 'T.Text' instead of 'Prelude.String'

__3 - Bytes__

  * "Data.ByteString" - 'BS.ByteString' is a chunk of bytes
  * "Data.Text.Encoding" - Conversions between 'BS.ByteString' and 'T.Text'

__4 - Sockets__

  * "Network.Socket" - Opening and closing sockets, resolving network addresses
  * "Network.Socket.ByteString" - Writing to and reading from sockets

__5 - HTTP__

  * "ASCII", "ASCII.Char" - Expressing HTTP messages as strings using the 'A.string' quasi-quoter
  * "Network.Simple.TCP" - Listening for client connections using 'Net.serve'

__6 - HTTP types__

  * "ASCII.Decimal", "Data.ByteString.Lazy" - Defining datatypes for the parts of an HTTP message

__7 - Encoding__

  * "Data.Text.Lazy",  "Data.Text.Lazy.Builder" - Efficient text concatenations with 'TB.Builder'
  * "Data.ByteString.Lazy", "Data.ByteString.Builder" - Efficient byte string concatenations with 'BSB.Builder'
  * "Data.Time" - Simple performance testing with 'Time.getCurrentTime' and 'Time.diffUTCTime'

__8 - Responding__

  * No additional imports in this chapter

__9 - Content types__

  * "Data.Text.Lazy.Builder.Int", "Data.Text.Lazy.IO",  "Data.Text.Lazy.Encoding" - Building a text response body
  * "Text.Blaze.Html", "Text.Blaze.Html5", "Text.Blaze.Html.Renderer.Utf8" - Building an HTML response body
  * "Data.Aeson", "Data.Aeson.Key", "Data.Aeson.KeyMap" - Building a JSON response body

__10 - Change__

  * "Control.Concurrent.STM" - Shared 'STM.TVar' state for request-handling threads
  * "Control.Concurrent.Async" - Demonstrating thread safety with 'Async.replicateConcurrently_'

__11 - Streaming__

  * "Control.Concurrent" - Slowing things down with 'Concurrent.threadDelay'

__12 - ListT IO__

  * "List.Transformer" - Representing files and chunked HTTP message bodies as I/O streams

__13 - Parsing__

  * "Data.Map.Strict" - Mapping resource names to file paths
  * "Data.Attoparsec.ByteString" - Parsing HTTP requests

__14 - Errors__

  * "Unfork" - Thread-safe logging

The remaining chapters are in progress:

__15 - Reading the head__

__16 - Reading the body__

__17 - Connection reuse__

-}

{- $libraries

Re-exported modules, grouped by the package that each module originally comes from:

__ascii__ - "ASCII", "ASCII.Char", "ASCII.Decimal"

__aeson__ - "Data.Aeson", "Data.Aeson.Key", "Data.Aeson.KeyMap"

__async__ - "Control.Concurrent.Async"

__base__

  * File handles - "System.IO"
  * Fundamental data types - "Data.Char"
  * Miscellanea - "Control.Concurrent", "Control.Monad"

__blaze-html__ - "Text.Blaze.Html", "Text.Blaze.Html5", "Text.Blaze.Html5.Attributes", "Text.Blaze.Html.Renderer.Utf8"

__bytestring__

  * Strict - "Data.ByteString"
  * Lazy - "Data.ByteString.Lazy"
  * Builder - "Data.ByteString.Builder"
  * "Data.ByteString.Char8" - To discuss why we don't use it

__containers__ - "Data.Map.Strict"

__directory__ - "System.Directory"

__filepath__ - "System.FilePath"

__list-transformer__ - "List.Transformer"

__network__

  * "Network.Socket" - The Socket type, operations for opening and closing sockets
  * "Network.Socket.ByteString" - Socket read/write operations with strict byte strings

__network-simple__

  * "Network.Simple.TCP" - Sockets that listen for incoming client connections

__relude__ - "Relude"

__resourcet__ - "Control.Monad.Trans.Resource"

__safe-exceptions__ - "Control.Exception.Safe"

__stm__ - "Control.Monad.STM", "Control.Concurrent.STM", "Control.Concurrent.STM.TVar"

__text__

  * Strict - "Data.Text", "Data.Text.Encoding", "Data.Text.IO"
  * Lazy - "Data.Text.Lazy", "Data.Text.Lazy.Encoding", "Data.Text.Lazy.IO"
  * Builder - "Data.Text.Lazy.Builder", "Data.Text.Lazy.Builder.Int"

__time__ - "Data.Time"

__unfork__ - "Unfork"

-}
