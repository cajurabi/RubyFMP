#the first 12 bytes are as follows:
#4bytes ASCII = 'FORM'
#long int = size of file less header (which is filesize-8)
#4bytes ASCII = 'FMAP'
#NOTE: The chunk size long ints like the one above are stored in Motorola format, NOT Intel. You will have to byteswap to get the correct value, 
#ie: Bytes 1,2,3,4 need to become 4,3,2,1.
#
#The chunks in the file follow on one after the other, and consist of an 8byte header, and the information specific to that chunk. 
#See how the playback source reads in the information. The chunks can be in any order, and some chunks may not be used in a particular file. 
#Also, don't rely on chunks being a certain size, for example the MPHD is now 4 bytes bigger than in the last version
#
#Chunk header:
#4bytes ASCII = ChunkID (example: 'MPHD')
#long int = size of chunk data less header
#
#These are the chunks as of V1.2:
#ATHR - Up to 4 ASCII strings of author information, separated by 0 values, always an even size.
#MPHD - Map header, see struct in the editor source download
#EDHD - Editor information, see struct in mappy.c
#CMAP - Colour palette for 8bit maps, red byte, green byte, blue byte for however many colours are needed (so usually 256*3 bytes).
#BKDT - Block data. Contains BLKSTR structures for however many block structures were made.
#ANDT - Animation data. Contains ANISTR structures for however many animation structures were made, and also animation data.
#BGFX - The raw graphics in whatever format the map is in. Examples: 8bit: mapwidth*mapheight bytes per block, in forward 
#format *numblocks 16bit: mapwidth*mapheight*2 bytes per block, each word contains 5 bits red, 6 bits green, 5 bits blue.
#BODY - An array of short ints containing positive offsets into BKDT, and negative offsets into ANDT.
#LYR? - Where ? is an ASCII number form 1 to 7. These are the same size and format as BODY, and allow object layers to be used.
#You can add your own chunks to a map file, if you load it into mappy, when you save it, those additional chunks will be saved in the file, 
#but not necessarily in the same place as before.
#
#FMP1.0 notes:
#This is very similar, but the values in all the chunks refer to units rather than bytes, ie. in BODY 0,32,64,96 would be 0,1,2,3 in FMP1.0. 
#
#--------------------------------------------------------------------------------
#
#
#The MAP file format
#  MAP is a user definable format which you can use for basic maps. To define the format you need to change the maptype line in the mapwin.ini file to how you want it. It is also recommended to change importskip=0. The values are between the quotes and the default: "LW4H4A4-1" is the usual format for CDX maps. What is this cryptic rubbish? The first letter is either L or M and specifies the endianness of the MAP file, for Intel this is L, for Motorola M. The next parts are letters followed by numbers, currently you can have:
#
#W = Width of map in blocks, the next number is size of this field in bytes
#H = Height of map in blocks, the next number is size of this field in bytes
#A = map array, the next number is size of each cell in bytes, the number after that is the adjuster, if the blocks look wrong, you can adjust them up or down to match them up, -1 normally compensates. The array is stored in plain number=block form and all layers will be saved consecutively.
#
#  So, for example, if I ONLY wanted the map array in bytes I might use "LA1-1". This means L=Intel format, A1-1=Array 1 byte per cell, with -1 adjustment. If I wanted to be strange I could have "MW2W4H4H2A20" which is M=Motorola format, W2=Width as 16bit, W4=Width as 32bit, H4=Height as 32bit, H2=Height as 16bit, A20=Array 16bits per cell, 0 adjustment.
#
#  It is very important to set the other 'map' values correctly in the mapwin.ini file.
#maptype = "LW4H4A4-1"
#It is vital to set mapdefw to the width of your map (in blocks) when working with MAP files which DON'T contain map width (W)
#mapdefw = 100
#It is vital to set mapdefh to the height of your map (in blocks) when working with MAP files which DON'T contain map height (H)
#mapdefh = 100
#It is vital to set mapdefbw to the width of your tiles (in pixels) when working with MAP files, the exception is when it is 0 where a requester will be used every time a .MAP file is opened
#mapdefbw = 32
#It is vital to set mapdefbh to the height of your tiles (in pixels) when working with MAP files
#mapdefbh = 32
#It is useful to set mapstaggerx to the odd row offset (in pixels) when working with Isometric MAP files
#mapstaggerx = 0
#It is useful to set mapstaggery to the odd column offset (in pixels) when working with Isometric MAP files
#mapstaggery = 0
#It is useful to set mapclickmask to the block for the mask when working with Isometric MAP files
#mapclickmask = 0
#If you want to, set this to your tiles bitmap for MAP files, see Using Mappy as a simple map editor
#mapdefBMP = "nodefault.bmp" 
#
#
#--------------------------------------------------------------------------------
#
#
#Glossary
# 
#
#  Anim Structure a 16 byte structure and associated list of frames (Block Structure offsets) of unknown length
#  Block I have tried to make 'block' mean the block structures for this documentation
#  Block Editor The window that allows you to select and edit both still and animated blocks. Right clicking the mouse on it toggles between still/animated
#  Block Structure a 32 bytes structure that acts as information about a particular block, it is referenced by the Map Array and Anim Structures, and in turn references the raw Tile Graphics
#  BMP Standard Windows graphic file format, MappyWin32 only imports and exports 8bit and 24bit uncompressed versions (Other colour depths are imported/exported as 24bit) currently
#  FMP Flexible MaP format, a collection of all the information needed to render and animate a map for a game.
#  FMA A FMP file, but without the graphics (BGFX).
#  MAR A map array file.
#  MAP MAP format, a very basic user defined format for compatibility with other systems
#  Graphics Blocks See 'Tile'
#  Map Array an array of short int (16bit) values that reference Block Structures (when positive) and Anim Structures (when negative). There can be up to 8 layers of this array by using the Map Layers
#  Map Editor The window where you fill in the map array using the various tools provided
#  Map Layers There can be up to 8 layers of the map array for objects, ingame changes, etc. Normally you would only have 1 or 2 layers. These can be selected and changed in the Playback Libraries
#  Mappy A 2D tile map editor with lots of features, this is the Win32 version but there is also a DOS and WinAllegro version
#  Playback Library these are available separately for free at the Mappy homepage, there are several versions for different platforms, they provide an easy way to access the map in your game
#  Tile I have tried to make 'tile' mean the raw graphics the block structures use for this documentation
#  Tile Graphics All the graphics information for the map
#
#
#
#
#
#Function reference:
#
#-------------------------------------------------
#
#IMPORTANT: You must have set_gfx_mode BEFORE calling these:
#
#Note: 'mapname' can be any FMP file, or FMP file in an Allegro datafile
#such as "MYMAP.FMP" or "mpgame.dat#GAMEMAP"
#
#int MapLoad (char * mapname);
#Tries to load the .fmp file specified in mapname, eg.
#
#MapLoad ("MYMAP.FMP");
#Returns 0 on success, -1 on failure. You need to call this or MapDecode
#before using any of the other map functions. This is the same as the

#function MapLoadABM.
#
#int MapLoadVRAM (char * mapname);
#Tries to load the .fmp using video bitmaps to take advantage of hardware
#acceleration. If it fails, check maperror for the reason. See pbdemo2.c
#
#int MapLoadABM (char * mapname);
#
#
#
#Loads the .fmp with the graphics in Allegro memory BITMAPs, you can then
#get at the graphics with the abmTiles array, eg. if you want to get at 
#the third BITMAP, use abmTiles[2]. All calls to MapDraw functions will
#then use the allegro blit and masked_blit functions.
#
#
#-------------------------------------------------
#
#int MapDecode (unsigned char * mapmempt);
#
#Same as MapLoad, but loads from an area of memory containing an FMP file.
#(Useful if your maps are stored compressed, just uncompress to memory and
#call MapDecode).
#
#
#int MapDecodeVRAM (unsigned char * mapmempt);
#
#See MapLoadVRAM
#
#
#int MapDecodeABM (unsigned char * mapmempt);
#
#See MapLoadABM
#
#-------------------------------------------------
#
#int MapGenerateYLookup (void);
#
#This is optional, call this after MapLoad or MapDecode (or variant) and
#it will generate a lookup table that _may_ slightly speed up the functions
#
#MapGetBlock and MapSetBlock, and allows you to use the maparraypt. If you
#use this, you _must_ use MapChangeLayer if you want to swap between layers.
#Another advantage is you can access block or anim offsets simply with:
#
#maparraypt[y][x];
#
#Where x is the offset from the left in _BLOCKS_, and y is the offset from
#the top in _BLOCKS_. Note you can get the actual block structure with the
#
#functions MapGetBlock and MapSetBlock.
#The memory allocated is freed when either MapFreeMem is called, or another
#map is loaded.
#
#-------------------------------------------------
#
#int MapGetBlockID (int blid, int usernum)
#
#returns the number of the first block that matches 'blid' in the field 
#specified with usernum (1 to 7, user1 to user7). If no match, returns -1
#
#-------------------------------------------------
#
#int MapLoadMAR (char * filename, int layer);
#
#int MapDecodeMAR (unsigned char * marmemory, int layer);
#
#Loads a .MAR file into a specified layer. Decode does it from memory, 
#you can dealloc that memory afterwards. If you called MapGenerateYLookup,
#call it again after this function. Returns -1 on error, 0 on success.
#
#-------------------------------------------------
#
#int MapChangeLayer (int)
#
#This changes to a different map layer, returns -1 if failed, or the new
#layer number if success. See mpgame.c
#
#-------------------------------------------------
#
#int MapGetXOffset (int x, int y)
#
#int MapGetYOffset (int x, int y)
#
#These functions are handy for translating a pixel coordinate to block
#coordinates, especially for non-rectangular blocks (see mousecrd.c).
#
#-------------------------------------------------
#
#BLKSTR * MapGetBlockInPixels (int x, int y)
#
#Returns a BLKSTR pointer, useful for collision detection and examining a
#blockstructure. This is more useful on non-rectangular maps as it is pixel
#perfect.
#
#
#-------------------------------------------------
#
#BLKSTR * MapGetBlock (int x, int y);
#
#Returns a BLKSTR pointer, useful for collision detection and examining a
#blockstructure. Note: the x and y paramaters are the offset from the left
#and top of the map in _BLOCKS_ NOT pixels. See mpgame.c for an example.
#
#-------------------------------------------------
#
#void MapSetBlockInPixels (int x, int y, int strvalue)
#
#The x and y paramaters are the offset from the left and top of the map
#in pixels. If strvalue is positive, the cell is set to that block structure.
#If strvalue is negative the cell is set to that anim structure-1 (ie if
#you want to put anim 3 in, strvalue would be -4).
#
#
#-------------------------------------------------
#
#void MapSetBlock (int x, int y, int strvalue);
#
#The x and y paramaters are the offset from the left and top of the map
#in _BLOCKS_ NOT pixels. See mpgame.c for an example. If strvalue is
#positive, the cell is set to that block structure. If strvalue is
#negative the cell is set to that anim structure-1 (ie if you want to
#put anim 3 in, strvalue would be -4).
#
#-------------------------------------------------
#
#void MapRestore (void);
#
#Restores the graphics to video bitmaps in hardware rendering.
#
#-------------------------------------------------
#
#void MapCorrectColours (void);
#
#NOTE: This function is now redundant and does nothing. Maps are
#colour corrected on loading.
#
#-------------------------------------------------
#
#void MapSetPal8 (void);
#
#Only useful in 8bit (256 colour) mode. Sets the screen palette to the
#values contained int the map file.
#
#-------------------------------------------------
#
#void MapFreeMem (void);
#
#When you've had enough of the map, this frees all the memory mappypb
#has used.
#
#-------------------------------------------------
#
#void MapInitAnims (void);
#
#Call to reset the animated blocks to their defaults
#
#-------------------------------------------------
#
#void MapUpdateAnims (void);
#
#Animation control. Call from your game loop, moves blocks to next anim
#
#-------------------------------------------------
#
#void MapDrawBG (BITMAP * mapdestpt, int mapxo, int mapyo, int mapx, int mapy,
#
#  int mapw, int maph);
#
#
#Draws the blocks referenced in bgoff. Will clip the map to the specified area.
#Note: the normal clipping on the bitmap is ignored, so trying to draw off the
#edges of a bitmap will crash your programme.
#
#
#BITMAP * mapdestpt  pointer to a MEMORY bitmap to draw on (not screen)
#int mapxo           offset, in pixels, from left edge of map, this is
#                    the first visible column of pixels you are drawing
#int mapyo           offset, in pixels, from top edge of map, this is
#                    the first visible row of pixels you are drawing
#int mapx            offset, in pixels, from left edge of bitmap
#int mapy            offset, in pixels, from top edge of bitmap
#int mapw            width of area you want to draw
#int maph            height of area you want to draw
#
#-------------------------------------------------
#
#void MapDrawBGT (BITMAP * mapdestpt, int mapxo, int mapyo, int mapx, int mapy,
#
#  int mapw, int maph, int mapfg);
#
#Same as MapDrawBG, except colour 0 pixels are not drawn (8bit), or pink colour
#pixels are not drawn (other depths) (transparent).
#
#-------------------------------------------------
#
#void MapDrawFG (BITMAP * mapdestpt, int mapxo, int mapyo, int mapx, int mapy,
#  int mapw, int maph, int mapfg);
#
#Same as MapDrawBG, except:
#
#int mapfg           The foreground layer you want to draw, 0, 1, or 2
#
#-------------------------------------------------
#
#void MapDrawRow (BITMAP * mapdestpt, int mapxo, int mapyo, int mapx, int mapy,
#  int mapw, int maph, int maprw, void (*cellcall) (int cx, int cy, int dx, int dy))
#
#Same as MapDrawBG, except:
#
#int maprw is the row to draw
#cellcall is a callback function for each cell, pass NULL if you are doing your
#own depth sorting, or using another system
#see isodemo.c for an example of this
#
#-------------------------------------------------
#
#BITMAP * MapMakeParallaxBitmap (BITMAP * sourcebm, int style);
#
#Only on regular rectangular maps.
#Pass a bitmap you would like to use for the parallax bitmap. Paramater 2
#is where you want the bitmap created, 0 means it is created with
#'create_bitmap', 1 means it is created with 'create_video_bitmap'. You
#must use this function to make the bitmap you pass to MapDrawParallax.
#The source bitmap MUST be a multiple of your block size (ie if you are
#using 32*32 blocks, 128*128 and 96*64 are valid, but 100*100 is not).
#
#-------------------------------------------------
#
#void MapDrawParallax (BITMAP * mapdestpt, BITMAP * parbm, int mapxo, int mapyo, int mapx, int mapy,#
#  int mapw, int maph);
#
#This behaves like MapDrawBG etc, except the 'parbm' bitmap is created
#with 'MapMakeParallaxBitmap' and this is tiled in the transparent
#regions of the map. After calling this you should call 'MapDrawBGT'
#instead of 'MapDrawBG' so the parallax layer isn't overwritten.
#This is more efficient than drawing the whole screen as it only draws
#in areas not obscured by higher layers, there is minimal overdraw
#(pixels aren't overwritten by higher layers where possible).
#
#See the source 'pbdemo3.c' for an example.
#
#-------------------------------------------------
#

require "stringio"

$stdout.sync = true

class RubyFMAP  
  attr_reader :mapVersion, :mapLittleEndian, :mapType, :mapWidth, :mapHeight, :mapBlockWidth, :mapBlockHeight, :mapBitDepth, :mapBlockStructSize
  attr_reader :mapNumBlockStruct, :mapNumBlockGfx
  attr_reader :mapColorKey8bit, :mapColorKeyRed, :mapColorKeyGreen, :mapColorKeyBlue
  attr_reader :mapBlockgapx, :mapBlockgapy, :mapBlockStaggerX, :mapBlockStaggerY
  attr_reader :mapClickMask, :mapIsoPillars
  attr_reader :sColorPalette, :aBlocks
  
  
  def initialize
    # Create struct
    @Block_S = Struct.new(:iBGOffset, :aiFGOffset, :aoUser, :bTopLeft, :bTopRight, :bBottomLeft, :bBottomRight, :bTrigger)
    @Animation_S = Struct.new(:iType, :iDelay, :iCount, :iUser, :iCurOffset, :iStartOffset, :iEndOffset)

    @aiLayers = Array.new(8) { Object.new }
  end

  protected
  @iChunkSize
  @short
  @long

  def decodeANDTG(iChunkSize)
#      animstrpt = malloc (ANDTMEM);
#  if (animstrpt == NULL) { merror = ER_OUTOFMEM; return -1; }
#  memset (animstrpt, 0, ANDTMEM);
#
#  aniadjust = 0;
#  myanistrpt = (ANISTR *) (mdatendpt);
#  while (1) {
#    aniadjust ++; //= (16-(sizeof(ANISTR)));
#    myanistrpt--;
#    if (myanistrpt->antype == AN_END) break;
#  }
#
#  logit ("Aniadjust = %d\n", aniadjust);
#
#  myanistrpt = (ANISTR *) (animstrpt+ANDTMEM);
#  while (1) {
#    myanistrpt--;
#    mdatendpt -= 16;
#    myanistrpt->antype = mdatendpt[0];
#    myanistrpt->andelay = mdatendpt[1];
#    myanistrpt->ancount = mdatendpt[2];
#    myanistrpt->anuser = mdatendpt[3];
#
#    if (maptype == 0) {
#    myanistrpt->ancuroff = (int) MapGetlong (mdatendpt+4)+(aniadjust*(16-sizeof(ANISTR)));
#    myanistrpt->anstartoff = (int) MapGetlong (mdatendpt+8)+(aniadjust*(16-sizeof(ANISTR)));
#    myanistrpt->anendoff = (int) MapGetlong (mdatendpt+12)+(aniadjust*(16-sizeof(ANISTR)));
#    } else {
#    myanistrpt->ancuroff = (MapGetlong(mdatendpt+4))*(sizeof(int));
#    myanistrpt->ancuroff -= MapGetchksz(mdatpt-4);
#    myanistrpt->anstartoff = (MapGetlong(mdatendpt+8))*(sizeof(int));
#    myanistrpt->anstartoff -= MapGetchksz(mdatpt-4);
#    myanistrpt->anendoff = (MapGetlong(mdatendpt+12))*(sizeof(int));
#    myanistrpt->anendoff -= MapGetchksz(mdatpt-4);
#    }
#
#  logit ("anim: type=%d delay=%d count=%d cur=%d strt=%d end=%d\n",
#    myanistrpt->antype, myanistrpt->andelay, myanistrpt->ancount,
#      myanistrpt->ancuroff, myanistrpt->anstartoff, myanistrpt->anendoff);
#    if (myanistrpt->antype == AN_END) break;
#  }
#
#  myseqpt = (int *) myanistrpt;
#  while (mdatendpt > mdatpt) {
#    myseqpt--;
#    mdatendpt -= 4;
#    if (maptype == 0)
#      *myseqpt = (int) ((MapGetlong (mdatendpt)/blockstrsize)*sizeof(BLKSTR));
#    else
#      *myseqpt = (int) ((MapGetlong (mdatendpt))*sizeof(BLKSTR));
#  }
#  InitAnims ();
#  return 0;
#}
  
  end
  
  
  def decodeATHR(iChunkSize)
    ### Read the author strings
    arrayString = @sioFMap.read(iChunkSize).split(/\000/)
    
    puts arrayString[0].to_s  
    puts arrayString[1].to_s  
    puts arrayString[2].to_s  
    puts arrayString[3].to_s  
  end
  
  
  def decodeBGFX(iChunkSize)
    iStartPos = @sioFMap.pos
    @sBGFX = @sioFMap.read(iChunkSize)   
    raise "Truncated BGFX data: unable to read complete chunk.  Attempted #{iChunkSize} bytes, yet only read #{@sioFMap.pos - iStartPos}" if @sioFMap.pos != iStartPos + iChunkSize
  end
  
  
  def decodeBKDT(iChunkSize)
    ### Ensure iChunkSize is multiple of mapBlockStructSize and that iChunkSize matches the @mapNumBlockStruct
    raise "Blockdata corrupt: chunk size (#{iChunkSize}) not divisble by #@mapBlockStructSize" if iChunkSize % @mapBlockStructSize != 0
    raise "Blockdata corrupt: number of blocks (#{(iChunkSize/@mapBlockStructSize)}) does not match header specified number of blocks (#@mapNumBlockStruct)" if (iChunkSize / @mapBlockStructSize) != @mapNumBlockStruct

    ### Block data (should contain number of block structures
    @aBlocks = Array.new(@mapNumBlockStruct) { @Block_S.new }
    
    ### Create FG offset array
    aiFGOffset = Array.new(3) { Integer }
    
    ### Create user array
    aoUser = Array.new(7) { Object.new }
    
    iStopPos = @sioFMap.pos + iChunkSize    
    iCurBlock = 0
    
    until @sioFMap.pos >= iStopPos do
      iStartPos = @sioFMap.pos
      
      iBGOffset = @sioFMap.read(4).unpack(@long)[0]
      aiFGOffset[0] = @sioFMap.read(4).unpack(@long)[0]
      aiFGOffset[1] = @sioFMap.read(4).unpack(@long)[0]
      aiFGOffset[2] = @sioFMap.read(4).unpack(@long)[0]
    
      if @mapType > 0
        ### compute block size
        iBlockSize = @mapBlockWidth * @mapBlockHeight * (@mapBlockBitDepth / 8)

        iBGOffset     *= iBlockSize
        aiFGOffset[0] *= iBlockSize
        aiFGOffset[1] *= iBlockSize
        aiFGOffset[2] *= iBlockSize        
      end
      
      aoUser[0] = @sioFMap.read(4).unpack(@long)[0]
      aoUser[1] = @sioFMap.read(4).unpack(@long)[0]
      aoUser[2] = @sioFMap.read(2).unpack(@short)[0]
      aoUser[3] = @sioFMap.read(2).unpack(@short)[0]
      aoUser[4] = @sioFMap.read(1).unpack('C')[0]
      aoUser[5] = @sioFMap.read(1).unpack('C')[0]
      aoUser[6] = @sioFMap.read(1).unpack('C')[0]
      
      sBits         = @sioFMap.read(1).unpack('b8')
      bTopLeft      = sBits[0] == '1'
      bTopRight     = sBits[1] == '1'
      bBottomLeft   = sBits[2] == '1'
      bBottomRight  = sBits[3] == '1'      
      bTrigger      = sBits[4] == '1'

      @aBlocks[iCurBlock] = @Block_S.new(iBGOffset, aiFGOffset.clone, aoUser.clone, bTopLeft, bTopRight, bBottomLeft, bBottomRight, bTrigger)
      if @sioFMap.pos < iStartPos + @mapBlockStructSize
        puts "Current pos #{@sioFMap.pos} need to go to #{iStartPos + @mapBlockStructSize}"
        @sioFMap.seek(iStartPos + @mapBlockStructSize) 
      end
      
      iCurBlock += 1
    end
  
  end
  
  
  def decodeBODY(iChunkSize)
    self.send('decodeLayer', iChunkSize, 0)  
  end
  
  
  def decodeCMAP(iChunkSize)
    ### check to ensure palette is 256 * 3 bytes long
    raise "Unsupported palette: palette size (#{iChunkSize}) is larger than 768 bytes in #@sFilename" if iChunkSize != 768
    raise "Unsupported palette: palette size (#{iChunkSize}) is not divisible by 3 in #@sFilename" if iChunkSize % 3 != 0
    @sColorPalette = @sioFMap.read(iChunkSize)
  end
  
  
  def decodeLayer(iChunkSize, iLayer)
    raise "Layer corrupted: layer #{iLayer} size (#{iChunkSize}) is smaller than #{@mapHeight * @mapWidth * 2} bytes in #@sFilename" if iChunkSize < @mapHeight * @mapWidth * 2
    raise "Layer corrupted: layer #{iLayer} size (#{iChunkSize}) is larger than #{@mapHeight * @mapWidth * 2} bytes in #@sFilename" if iChunkSize > @mapHeight * @mapWidth * 2

    ### Get current position
    iStartPos = @sioFMap.pos

    @aiLayers[iLayer] =  Array.new(@mapHeight * @mapWidth) { Integer }

    case @mapType
    when 0 then
      for iFig in (0..(@mapHeight * @mapWidth - 1)) do
        iValue = @sioFMap.read(2).unpack(@short)[0]

        if iValue >= 0
          iValue /= @mapBlockStructSize
        end

        @aiLayers[iLayer][iFig] = iValue
      end

    when 1 then
      for iFig in (0..(@mapHeight * @mapWidth - 1)) do
        iValue = @sioFMap.read(2).unpack(@short)[0]

        if iValue < 0
          iValue *= 16
        end

        @aiLayers[iLayer][iFig] = iValue
      end

    when 2 then
      iKiwi = 0
      for iFig in (0..(@mapHeight * @mapWidth - 1)) do
        iCount = @sioFMap.read(2).unpack(@short)[0]

        if iCount > 0
          while iCount != 0
            iValue = @sioFMap.read(2).unpack(@short)[0]

            if iValue < 0
              iValue *= 16
            end

            @aiLayers[iLayer][iKiwi] = iValue

            iKiwi  += 1
            iCount -= 1
          end
        elsif iCount < 0
          iValue = @sioFMap.read(2).unpack(@short)[0]

          if iValue < 0
            iValue *= 16
          end

          while iCount != 0
            @aiLayers[iLayer][iKiwi] = iValue

            iKiwi  += 1
            iCount += 1
          end
        else
          ### Raise exception if we encounter a map entry with a value of 0
          raise "Unsupported layer: value (0) found in map type #@mapType in #@sFile, layer #{iLayer} position " + @sioFMap.pos
        end
      end

    when 3 then
      iKiwi = 0
      for iFig in (0..(@mapHeight * @mapWidth - 1)) do
        iCount = @sioFMap.read(2).unpack(@short)[0]

        if iCount > 0
          while iCount != 0
            iValue = @sioFMap.read(2).unpack(@short)[0]

            if iValue < 0
              iValue *= 16
            end

            @aiLayers[iLayer][iKiwi] = iValue

            iKiwi  += 1
            iCount -= 1
          end
        elsif iCount < 0
          iIdx = @sioFMap.read(2).unpack(@short)[0]

          while iCount != 0
            @aiLayers[iLayer][iKiwi] = @aiLayers[iLayer][iKiwi + iIdx]

            iKiwi  += 1
            iCount += 1
          end
        else
          ### Raise exception if we encounter a map entry with a value of 0
          raise "Unsupported layer: value (0) found in map type #@mapType in #@sFile, layer #{iLayer} position " + @sioFMap.pos
        end
      end
    end

    ### Raise exception if we did not read all of the map header data
    raise "Unsupported layer: chunk size is incorrect (#{iChunkSize}) in #@sFile" if @sioFMap.pos - iStartPos - iChunkSize != 0

  end

  
  def decodeMPHD(iChunkSize)
    ### Ensure blocksize is correct
    raise "Unsupported map header: chunk size is too small (#{iChunkSize}) in #@sFilename" if iChunkSize < 24

    ### Get current position
    iStartPos = @sioFMap.pos
    
    ### Map Version stored major:minor
    @mapVersion = @sioFMap.read(1).unpack('C')[0] + ('0.' + @sioFMap.read(1).unpack('C').to_s).to_f
    puts "Map version: #@mapVersion"
    raise "Unsupported map version: #@mapVersion" if @mapVersion > 1.0
    
    @mapLittleEndian = @sioFMap.read(1).unpack('C')[0] == 1
    if @mapLittleEndian
      @short = 'v'
      @long  = 'V'
    else
      @short = 'n'
      @long  = 'N'      
    end    
    puts "Map endian: #@mapLittleEndian"

    ### mapType = 0 - 32 byte offset
    ### mapType != 0 - 16 byte offset
    @mapType = @sioFMap.read(1).unpack('C')[0]
    puts "Map type: #@mapType"
    raise "Unsupported map type: #@mapType" if @mapType > 3
    
    @mapWidth = @sioFMap.read(2).unpack(@short)[0]
    @mapHeight = @sioFMap.read(2).unpack(@short)[0]
    puts "Map width: #@mapWidth"
    puts "Map height: #@mapHeight"

    ### Skip 4 reserved bytes
    @sioFMap.read(4)

    ### Height and Width in pixels of the blocks
    @mapBlockWidth = @sioFMap.read(2).unpack(@short)[0]
    @mapBlockHeight = @sioFMap.read(2).unpack(@short)[0]
    puts "Block width: #@mapBlockWidth"
    puts "Block height: #@mapBlockHeight"

    ### Bitdepth of blocks
    @mapBitDepth = @sioFMap.read(2).unpack(@short)[0]
    raise "Unsupported bit depth: #@mapBitDepth in #@sFilename.  Only support 8, 16, and 24" if !(@mapBitDepth == 8 or @mapBitDepth == 16 or @mapBitDepth == 24) 
    puts "Block bit depth: #@mapBitDepth"

    ### Size of block struct size
    @mapBlockStructSize = @sioFMap.read(2).unpack(@short)[0]
    raise "Unsupported block struct size: block struct size (#@mapBlockStructSize) is less than 32" if @mapBlockStructSize < 32
    puts "Block struct size: #@mapBlockStructSize"
    
    ### Number of block structures in BKDT
    @mapNumBlockStruct = @sioFMap.read(2).unpack(@short)[0]
    puts "Number of blocks: #@mapNumBlockStruct"
 
    ### Number of blocks in graphics BODY
    @mapNumBlockGfx = @sioFMap.read(2).unpack(@short)[0]


    ### Color key values used for transparency
    if iChunkSize > 24
      @mapColorKey8bit = @sioFMap.read(1).unpack('C')[0]
      @mapColorKeyRed = @sioFMap.read(1).unpack('C')[0]
      @mapColorKeyGreen = @sioFMap.read(1).unpack('C')[0]
      @mapColorKeyBlue = @sioFMap.read(1).unpack('C')[0]
    else
      ### No transparency
      ### TODO: do I need to set???
    end
    
    ### Check for non-rectangular info
    if iChunkSize > 28
      @mapBlockgapx = @sioFMap.read(2).unpack(@short)[0]
      @mapBlockgapy = @sioFMap.read(2).unpack(@short)[0]
      @mapBlockStaggerX = @sioFMap.read(2).unpack(@short)[0]
      @mapBlockStaggerY = @sioFMap.read(2).unpack(@short)[0]
    else      
      @mapBlockgapx = @mapBlockWidth
      @mapBlockgapy = @mapBlockHeight
      @mapBlockStaggerX = 0
      @mapBlockStaggerY = 0
    end
  
    ### Check for clickmask
    ### TODO: how is click mask used?
    if iChunkSize > 36
      @mapClickMask = @sioFMap.read(2).unpack(@short)[0]
    else
      @mapClickMask = 0
    end

    ### Check for Iso pillars
    if iChunkSize > 38
      @mapIsoPillars = @sioFMap.read(2).unpack(@short)[0]
    else
      @mapIsoPillars = 0
    end
    
    ### Raise exception if we did not read all of the map header data
    raise "Unsupported map header: chunk size is incorrect (#{iChunkSize}) in #@sFile" if @sioFMap.pos - iStartPos - iChunkSize != 0
    
  end


  public
  def loadFile(sFilename)
    @sFilename = sFilename
    @sioFMap = StringIO.new(IO.read(sFilename))

    if @sioFMap.read(4) == 'FORM'
      puts 'Form found!'
    
      iFMapSize = @sioFMap.read(4).unpack('N')[0]
      iFileSize = @sioFMap.size - 8
      
      if iFMapSize == iFileSize
        puts 'Block size matches'
      else
        raise "FMap: blocksize mismatch in #@sFilename"
      end
      
      # Check if FMAP
      if @sioFMap.read(4) == 'FMAP'
        puts 'Valid FMAP'
      else
        raise "Fmap: missing FMAP signature in #@sFilename"       
      end
      
      until @sioFMap.pos >= iFMapSize do
        begin
          self.send('decode' + @sioFMap.read(4), @sioFMap.read(4).unpack('N')[0])
        rescue NoMethodError
          puts $!

          @sioFMap.seek(@sioFMap.pos - 4)
          iChunkSize = @sioFMap.read(4).unpack('N')[0]
          puts 'skipping ' + iChunkSize.to_s
          
          @sioFMap.read(iChunkSize)
          retry
        rescue TypeError
          puts 'End of file:' + $!
        end
      end
    end    
  end
end



cFMAP = RubyFMAP.new()

cFMAP.loadFile("test.fmp")


