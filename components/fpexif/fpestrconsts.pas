unit fpeStrConsts;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes, SysUtils;

resourcestring

  // *** Error messages ***

  rsCannotSaveToUnknownFileFormat = 'The metadata structure cannot be saved because '+
    'the file format of the receiving file is not known or not supported.';
  rsFileNotFoundError = 'File "%s" not found.';
  rsImageDataFileNotExisting = 'File "%s" providing the image data does not exist.';
  rsImageDataFileNotSpecified = 'The metadata structure is not linked to an image. '+
    'Specify the name of the file providing the image data.';
  rsImageFormatNotSupported = 'Image format not supported.';
  rsImageResourceNameTooLong = 'Image resource name "%s" too long.';
  rsIncompleteJpegSegmentHeader = 'Defective JPEG structure: Incomplete segment header';
  rsIncorrectFileStructure = 'Incorrect file structure';
  rsIncorrectTagType = 'Incorrect tag type %d: Index=%d, TagID=$%.04x, File:"%s"';
  rsIptcDataExpected = 'IPTC data expected, but not found.';
  rsIptcExtendedDataSizeNotSupported = 'Data size %d not supported for an IPTC extended dataset.';
  rsJpegCompressedDataWriting = 'Writing error of compressed data.';
  rsJpegSegmentMarkerExpected = 'Defective JPEG structure: Segment marker ($FF) expected.';
  rsJpegReadWriteErrorInSegment = 'Read/write error in segment $FF%.2x';
  rsMoreThumbnailTagsThanExpected = 'More thumbnail tags than expected.';
  rsNoValidIptcFile = 'No valid IPTC file';
  rsNoValidIptcSignature = 'No valid IPTC signature';
  rsRangeCheckError = 'Range check error.';
  rsReadIncompleteIFDRecord = 'Read incomplete IFD record at stream position %d.';
  rsTagTypeNotSupported = 'Tag "%s" has an unsupported type.';
  rsUnknownImageFormat = 'Unknown image format.';
  rsWritingNotImplemented = 'Writing of %s files not yet implemented.';

  // general lookup values
  rsAutoManual = '0:Auto,1:Manual';
  rsEconomyNormalFine = '0:Economy,1:Normal,2:Fine';
  rsEconomyNormalFine1 = '1:Economy,2:Normal,3:Fine';
  rsLowNormalHigh = '0:Low,1:Normal,2:High';
  rsNormalLowHigh = '0:Normal,1:Low,2:High';
  rsNormalSoftHard = '0:Normal,1:Soft,2:Hard';
  rsNoYes = '0:No,1:Yes';
  rsOffOn = '0:Off,1:On';
  rsSingleContinuous = '0:Single,1:Continuous';

  // *** EXIF tags ***

  rsAcceleration = 'Acceleration';
//  rsActionAdvised = 'Action advised';
  rsAperturevalue = 'Aperture value';
  rsArtist = 'Artist';
  rsBitsPerSample = 'Bits per sample';
  rsBrightnessValue = 'Brightness value';
//  rsByLine = 'By-line';
//  rsByLineTitle = 'By-line title';
  rsCameraElevationAngle = 'Camera elevation angle';
//  rsCategory = 'Category';
  rsCellHeight = 'Cell height';
  rsCellWidth = 'Cell width';
  rsCFAPattern = 'CFA pattern';
//  rsCity = 'City';
//  rsCodedCharacterSet = 'Coded character set';
  rsColorSpace = 'Color space';
  rsColorSpaceLkup = '0:sBW,1:sRGB,2:Adobe RGB,65533:Wide Gamut RGB,65534:ICC Profile,65535:Uncalibrated';
  rsComponentsConfig = 'Components configuration';
  rsCompressedBitsPerPixel = 'Compressed bits per pixel';
  rsCompression = 'Compression';
  rsCompressionLkup = '1:Uncompressed,2:CCITT 1D,3:T4/Group 3 Fax,'+
    '4:T6/Group 4 Fax,5:LZW,6:JPEG (old-style),7:JPEG,8:Adobe Deflate,'+
    '9:JBIG B&W,10:JBIG Color,99:JPEG,262:Kodak 262,32766:Next,'+
    '32767:Sony ARW Compressed,32769:Packed RAW,32770:Samsung SRW Compressed,'+
    '32771:CCIRLEW,32772:Samsung SRW Compressed 2,32773:PackBits,'+
    '32809:Thunderscan,32867:Kodak KDC Compressed,32895:IT8CTPAD,'+
    '32896:IT8LW,32897:IT8MP,32898:IT8BL,32908:PixarFilm,32909:PixarLog,'+
    '32946:Deflate,32947:DCS,34661:JBIG,34676:SGILog,34677:SGILog24,'+
    '34712:JPEG 2000,34713:Nikon NEF Compressed,34715:JBIG2 TIFF FX,'+
    '34718:Microsoft Document Imaging (MDI) Binary Level Codec,'+
    '34719:Microsoft Document Imaging (MDI) Progressive Transform Codec,'+
    '34720:Microsoft Document Imaging (MDI) Vector,34892:Lossy JPEG,'+
    '65000:Kodak DCR Compressed,65535:Pentax PEF Compressed';
//  rsContact = 'Contact';
//  rsContentLocCode = 'Content location code';
//  rsContentLocName = 'Content location name';
  rsContrast = 'Contrast';
  rsCopyright = 'Copyright';
  rsCustomRendered = 'Custom rendered';
  rsCustomRenderedLkup = '0:Normal,1:Custom';
//  rsDateCreated = 'Date created';
  rsDateTime = 'Date/time';
  rsDateTimeOriginal = 'Date/time original';
  rsDateTimeDigitized = 'Date/time digitized';
  rsDeviceSettingDescription = 'Device setting description';
  rsDigitalZoom = 'Digital zoom';
  rsDigitalZoomRatio = 'Digital zoom ratio';
  rsDigitizeDate = 'Digital creation date';
  rsDigitizeTime = 'Digital creation time';
  rsDocumentName = 'Document name';
//  rsEditorialUpdate = 'Editorial update';
//  rsEditStatus = 'Edit status';
  rsExifImageHeight = 'EXIF image height';
  rsExifImageWidth = 'EXIF image width';
  rsExifOffset = 'EXIF offset';
  rsExifVersion = 'EXIF version';
//  rsExpireDate = 'Expiration date';
//  rsExpireTime = 'Expiration time';
  rsExposureBiasValue = 'Exposure bias value';
  rsExposureIndex = 'Exposure index';
  rsExposureMode = 'Exposure mode';
  rsExposureModeLkup = '0:Auto,1:Manual,2:Auto bracket';
  rsExposureProgram = 'Exposure program';
  rsExposureProgramLkup = '0:Not defined,1:Manual,2:Program AE,3:Aperture-priority AE,'+
    '4:Shutter speed priority AE,5:Creative (slow speed),6:Action (high speed),'+
    '7:Portrait,8:Landscape;9:Bulb';
  rsExposureTime = 'Exposure time';
  rsExtensibleMetadataPlatform = 'Extensible metadata platform';
  rsFileSource = 'File source';
  rsFileSourceLkup = '0:Unknown,1:Film scanner,2:Reflection print scanner,3:Digital camera';
  rsFillOrder = 'Fill order';
  rsFillOrderLkup = '1:Normal,2:Reversed';
//  rsFixtureID = 'Fixture ID';
  rsFlash = 'Flash';
  rsFlashEnergy = 'Flash energy';
  rsFlashLkup = '0:No flash,1:Fired,5:Fired; return not detected,'+
    '7:Fired; return detected,8:On; did not fire,9:On; fired,'+
    '13:On; return not detected,15:On; return detected,16:Off; did not fire,'+
    '20:Off; did not fire, return not detected,24:Auto; did not fire,'+
    '25:Auto; fired;29:Auto; fired; return not detected,31:Auto; fired; return detected,'+
    '32:No flash function,48:Off, no flash function,65:Fired; red-eye reduction,'+
    '69:Fired; red-eye reduction; return not detected,'+
    '71:Fired; red-eye reduction; return detected,73:On; red-eye reduction,'+
    '77:On; red-eye reduction, return not detected,'+
    '79:On, red-eye reduction, return detected,80:Off; red-eye reduction,'+
    '88:Auto; did not fire; red-eye reduction,89:Auto; fired; red-eye reduction,'+
    '93:Auto; fired; red-eye reduction; return not detected,'+
    '95:Auto; fired; red-eye reduction, return detected';
  rsFlashPixVersion = 'FlashPix version';
  rsFNumber = 'F number';
  rsFocalLength = 'Focal length';
  rsFocalLengthIn35mm = 'Focal length in 35 mm film';
  rsFocalPlaneResUnit = 'Focal plane resolution unit';
  rsFocalPlaneResUnitLkup = '1:None,2:inches,3:cm,4:mm,5:um';
  rsFocalPlaneXRes = 'Focal plane x resolution';
  rsFocalPlaneYRes = 'Focal plane y resolution';
  rsGainControl = 'Gain control';
  rsGainControlLkup = '0:None,1:Low gain up,2:High gain up,3:Low gain down,4:High gain down';
  rsGamma = 'Gamma';
  rsGPSAltitude = 'GPS altitude';
  rsGPSAltitudeRef = 'GPS altitude reference';
  rsGPSAltitudeRefLkup = '0: Above sea level,1:Below sea level';
  rsGPSAreaInformation = 'Area information';
  rsGPSDateDifferential = 'GPS date differential';
  rsGPSDateDifferentialLkup = '0:No correction,1:Differential corrected';
  rsGPSDateStamp = 'GPS date stamp';
  rsGPSDestBearing = 'GPS destination bearing';
  rsGPSDestBearingRef = 'GPS destination bearing reference';
  rsGPSDestDistance = 'GPS destination distance';
  rsGPSDestDistanceRef = 'GPS destination distance reference';
  rsGPSDestLatitude = 'GPS destination latitude';
  rsGPSDestLatitudeRef = 'GPS destination latitude reference';
  rsGPSDestLongitude = 'GPS destination longitude';
  rsGPSDestLongitudeRef = 'GPS destination longitude reference';
  rsGPSDistanceRefLkup = 'K:Kilometers,M:Miles,N:Nautical miles';
  rsGPSDOP = 'GPS DOP';
  rsGPSHPositioningError = 'GPS H positioning error';
  rsGPSImageDirection = 'GPS image direction';
  rsGPSImageDirectionRef = 'GPS image direction reference';
  rsGPSInfo = 'GPS info';
  rsGPSLatitude = 'GPS latitude';
  rsGPSLatitudeRef = 'GPS latitude reference';
  rsGPSLatitudeRefLkup = 'N:North,S:South';
  rsGPSLongitude = 'GPS longitude';
  rsGPSLongitudeRef = 'GPS longitude reference';
  rsGPSLongitudeRefLkup = 'E:East,W:West';
  rsGPSMapDatum = 'GPS map datum';
  rsGPSMeasureMode = 'GPS measurement mode';
  rsGPSMeasureModeLkup = '2:2-Dimensional Measurement,3:3-Dimensional Measurement';
  rsGPSProcessingMode = 'GPS processing mode';
  rsGPSSatellites = 'GPS satellites';
  rsGPSSpeed = 'GPS speed';
  rsGPSSpeedRef = 'GPS speed reference';
  rsGPSSpeedRefLkup = 'K:km/h,M:mph,N:knots';
  rsGPSStatus = 'GPS status';
  rsGPSTimeStamp = 'GPS time stamp';
  rsGPSTrack = 'GPS track';
  rsGPSTrackRef = 'GPS track reference';
  rsGPSTrackRefLkup = 'M:Magnetic north,T:True north';
  rsGPSVersionID = 'GPS version ID';
  rsHalftoneHints = 'Half-tone hints';
  rsHostComputer = 'Host computer';
  rsHumidity = 'Humidity';
//  rsImageCaption = 'Image caption';
//  rsImageCaptionWriter = 'Image caption writer';
//  rsImageCredit = 'Image credit';
  rsImageDescr = 'Image description';
//  rsImageHeadline = 'Image headline';
  rsImageHeight = 'Image height';
  rsImageHistory = 'Image history';
  rsImageNumber = 'Image number';
//  rsImageType = 'Image type';
  rsImageUniqueID = 'Unique image ID';
  rsImageWidth = 'Image width';
  rsInkSet = 'Ink set';
  rsInkSetLkup = '1:CMYK,2:Not CMYK';
  rsInteropIndex = 'Interoperabiliy index';
  rsInteropOffset = 'Interoperability offset';
  rsInteropVersion = 'Interoperability version';
  rsIPTCNAA = 'IPTC/NAA';
  rsISOSpeed = 'ISO speed';
  rsISOSpeedLatitudeYYY = 'ISO latitude yyy';
  rsISOSpeedLatitudeZZZ = 'ISO speed latitude zzz';
  rsISO = 'ISO';
  rsLensInfo = 'Lens info';
  rsLensMake = 'Lens make';
  rsLensModel = 'Lens model';
  rsLensSerialNumber = 'Lens serial number';
  rsLightSource = 'Light source';
  rsLightSourceLkup = '0:Unknown,1:Daylight,2:Fluorescent,3:Tungsten (incandescent),'+
    '4:Flash,9:Fine weather,10:Cloudy,11:Shade,12:Daylight fluorescent,'+
    '13:Day white fluorescent,14:Cool white fluorescent,15:White fluorescent,'+
    '16:Warm white fluorescent,17:Standard light A, 18:Standard light B,'+
    '19:Standard light C,20:D55,21:D65,22:D74,23:D50,24:ISO Studio tungsten,'+
    '255:Other';
  rsMacro = 'Macro';
  rsMake = 'Make';
  rsMakerNote = 'Maker note';
  rsMaxApertureValue = 'Max aperture value';
  rsMaxSampleValue = 'Max sample value';
  rsMeteringMode = 'Metering mode';
  rsMeteringModeLkup = '0:Unknown,1:Average,2:Center-weighted average,'+
    '3:Spot,4:Multi-spot,5:Multi-segment,6:Partial,255:Other';
  rsMinSampleValue = 'Min sample value';
  rsModel = 'Model';
  rsOffsetTime = 'Time zone for date/time';
  rsOffsetTimeOriginal = 'Time zone for date/time original';
  rsOffsetTimeDigitized = 'Time zone for date/time digitized';
  rsOrientation = 'Orientation';
  rsOrientationLkup = '1:Horizontal (normal),2:Mirror horizontal,3:Rotate 180,'+
    '4:Mirror vertical,5:Mirror horizontal and rotate 270 CW,6:Rotate 90 CW,'+
    '7:Mirror horizontal and rotate 90 CW,8:Rotate 270 CW';
  rsOwnerName = 'Owner name';
  rsPageName = 'Page name';
  rsPageNumber = 'Page number';
  rsPhotometricInt = 'Photometric interpretation';
  rsPhotometricIntLkup = '0:White is zero,1:Black is zero,2:RGB,3:RGB palette,'+
    '4:Transparency mask,5:CMYK,6:YCbCr,8:CIELab,9:ICCLab,10:ITULab,'+
    '32803:Color filter array,32844:Pixar LogL,32845:Pixar LogLuv,34892:Linear Raw';
  rsPlanarConfiguration = 'Planar configuration';
  rsPlanarConfigurationLkup = '1:Chunky,2:Planar';
  rsPredictor = 'Predictor';
  rsPredictorLkup = '1:None,2:Horizontal differencing';
  rsPressure = 'Pressure';
  rsPrimaryChromaticities = 'Primary chromaticities';
  rsQuality = 'Quality';
  rsRecExpIndex = 'Recommended exposure index';
  rsRefBlackWhite = 'Reference black & white';
  rsRelatedImageFileFormat = 'Related image file format';
  rsRelatedImageHeight = 'Related image height';
  rsRelatedImageWidth = 'Related image width';
  rsRelatedSoundFile = 'Related sound file';
  rsResolutionUnit = 'Resolution unit';
  rsResolutionUnitLkup = '1:None,2:inches,3:cm';
  rsRowsPerStrip = 'Rows per strip';
  rsSamplesPerPixel = 'Samples per pixel';
  rsSaturation = 'Saturation';
  rsSceneCaptureType = 'Scene capture type';
  rsSceneCaptureTypeLkup = '0:Standard,1:Landscape,2:Portrait,3:Night';
  rsSceneType = 'Scene type';
  rsSceneTypeLkup = '0:Unknown,1:Directly photographed';
  rsSecurityClassification = 'Security classification';
  rsSelfTimerMode = 'Self-timer mode';
  rsSEMInfo = 'SEM info';
  rsSensingMethod = 'Sensing method';
  rsSensingMethodLkup = '1:Not defined,2:One-chip color area,3:Two-chip color area,'+
    '4:Three-chip color area,5:Color sequential area,7:Trilinear,8:Color sequential linear';
  rsSensitivityType = 'Sensitivity type';
  rsSensitivityTypeLkup = '0:Unknown,1:Standard Output Sensitivity'+
    '2:Recommended exposure index,3:ISO speed,'+
    '4:Standard output sensitivity and recommended exposure index,'+
    '5:Standard output sensitivity and ISO Speed,6:Recommended exposure index and ISO speed,'+
    '7:Standard output sensitivity, recommended exposure index and ISO speed';
  rsSerialNumber = 'Serial number';
  rsSharpness = 'Sharpness';
  rsShutterSpeedValue = 'Shutter speed value';
  rsSoftware = 'Software';
  rsSpatialFrequResponse = 'Spatial frequency response';
  rsSpectralSensitivity = 'Spectral sensitivity';
  rsStdOutputSens = 'Standard output sensitivity';
  rsStripByteCounts = 'Strip byte counts';
  rsStripOffsets = 'Strip offsets';
  rsSubfileTypeLkup =
    '0:Full-resolution image,'+
    '1:Reduced-resolution image,'+
    '2:Single page of multi-page image,'+
    '3:Single page of multi-page reduced-resolution image,'+
    '4:Transparency mask,'+
    '5:Transparency mask of reduced-resolution image,'+
    '6:Transparency mask of multi-page image,'+
    '7:Transparency mask of reduced-resolution multi-page image';
  rsSubjectArea = 'Subject area';
  rsSubjectDistance = 'Subject distance';
  rsSubjectDistanceRange = 'Subject distance range';
  rsSubjectDistanceRangeLkup = '0:Unknown,1:Macro,2:Close,3:Distant';
  rsSubjectLocation = 'Subject location';
  rsSubSecTime = 'Fractional seconds of date/time';
  rsSubSecTimeOriginal = 'Fractional seconds of date/time original';
  rsSubSecTimeDigitized = 'Fractional seconds of date/time digitized';
  rsTargetPrinter = 'Target printer';
  rsTemperature = 'Temperature';
  rsThresholding = 'Thresholding';
  rsThresholdingLkup = '1:No dithering or halftoning,2:Ordered dither or halftone,'+
    '3:Randomized dither';
  rsThumbnailHeight = 'Thumbnail height';
  rsThumbnailOffset = 'Thumbnail offset';
  rsThumbnailSize = 'Thumbnail size';
  rsThumbnailWidth = 'Thumbnail width';
  rsTileLength = 'Tile length';
  rsTileWidth = 'Tile width';
  rsTimeZoneOffset = 'Time zone offset';
  rsTransferFunction = 'Transfer function';
  rsTransmissionRef = 'Original transmission reference';
  rsUserComment = 'User comment';
  rsWhiteBalance = 'White balance';
  rsWaterDepth = 'Water depth';
  rsWhitePoint = 'White point';
  rsXPosition = 'X position';
  rsXResolution = 'X resolution';
  rsYCbCrCoefficients = 'YCbCr coefficients';
  rsYCbCrPositioning = 'YCbCr positioning';
  rsYCbCrPosLkup = '1:Centered,2:Co-sited';
  rsYCbCrSubsampling = 'YCbCr subsampling';
  rsYPosition = 'Y position';
  rsYResolution = 'Y resolution';

  // *** MakerNote ***

  // Canon
  rsCanonAELkup = '0:Normal AE,1:Exposure compensation,2:AE lock,'+
    '3:AE lock + Exposure compensation,4:No AE';
  {
  rsCanonAFLkup = '12288:None (MF),12289:Auto-selected,12290:Right,12291:Center,'+
    '12292:Left';
    }
  rsCanonAFLkup = '$2005:Manual AF point selection,$3000:None (MF),' +
    '$3001:Auto AF point selection,$3002:Right,$3003:Center,$3004:Left,' +
    '$4001:Auto AF point selection,$4006:Face Detect';
  rsCanonAutoRotLkup = '0:None,1:Rotate 90 CW,2:Rotate 180,3:Rotate 270 CW';
  rsCanonBiasLkup = '65472:-2 EV,65484:-1.67 EV,65488:-1.50 EV,65492:-1.33 EV,'+
    '65504:-1 EV,65516:-0.67 EV,65520:-0.50 EV,65524:-0.33 EV,0:0 EV,'+
    '12:0.33 EV,16:0.50 EV,20:0.67 EV,32:1 EV,44:1.33 EV,48:1.50 EV,'+
    '52:1.67 EV,64:2 EV';
  rsCanonCamTypeLkup = '248:EOS High-end,250:Compact,252:EOS Mid-range,255:DV Camera';
  rsCanonEasyLkup = '0:Full Auto,1:Manual,2:Landscape,3:Fast Shutter,4:Slow Shutter,'+
    '5:Night,6:Gray scale,7:Sepia,8:Portrait,9:Sports,10:Macro,11:Black & White,'+
    '12:Pan Focus,13:Vivid,14:Neutral,15:Flash off,16:Long shutter,'+
    '17:Super macro,18:Foliage,19:Indoor,20:Fireworks,21:Beach,22:Underwater,'+
    '23:Snow,24:Kids & Pets,25:Night snapshot,26:Digital macro,27:My colors,'+
    '28:Movie snap,29:Super macro 2,30:Color accent,31:Color swap,32:Aquarium,'+
    '33:ISO3200,34:ISO6400,35:Creative light effect,36:Easy,37:Quick shot,'+
    '38:Creative auto,39:Zoom blur,40:Low light,41:Nostalgic,42:Super vivid,'+
    '43:Poster effect,44:Face self-timer,45:Smile,46:Wink self-timer,'+
    '47:Fisheye effect,48:Miniature effect,49:High-speed burst,'+
    '50:Best image selection,51:High dynamic range,52:Handheld night scene,'+
    '53:Movie digest,54:Live view control,55:Discreet,56:Blur reduction,'+
    '57:Monochrome,58:Toy camera effect,59:Scene intelligent auto,'+
    '60:High-speed burst HQ,61:Smooth skin,62:Soft focus,257:Spotlight,'+
    '258:Night 2,259:Night+,260:Super night,261:Sunset,263:Night scene,'+
    '264:Surface,265:Low light 2';
  rsCanonExposeLkup = '0:Easy shooting,1:Program AE,2:Shutter speed priority AE,'+
    '3:Aperture priority AE,4:Manual,5:Depth-of-field AE,6:M-Dep,7:Bulb';
  rsCanonFlashActLkup = '0:Did not fire,1:Fired';
  rsCanonFlashLkup = '0:Not fired,1:Auto,2:On,3:Red-eye,4:Slow sync,'+
    '5:Auto+red-eye,6:On+red eye,16:External flash';
  rsCanonFocalTypeLkup = '1:Fixed,2:Zoom';
  rsCanonFocTypeLkup = '0:Manual,1:Auto,3:Close-up (macro),8:Locked (pan mode)';
  rsCanonFocusLkup = '0:One-Shot AF,1:AI Servo AF,2:AI Focus AF,3:Manual focus,'+
    '4:Single,5:Continuous,6:Manual focus,16:Pan focus,256:AF+MF,'+
    '512:Movie snap focus,519:Movie servo AF';
  rsCanonGenLkup = '65535:Low,0:Normal,1:High';
  rsCanonImgStabLkup = '0:Off,1:On,2:Shoot only,3:Panning,4:Dynamic,256:Off,'+
    '257:On,258:Shoot only,259:Panning,260:Dynamic';
  rsCanonISOLkup = '0:Not used,15:auto,16:50,17:100,18:200,19:400';
  rsCanonMacroLkup = '1:Macro,2:Normal';
  rsCanonMeterLkup = '0:Default,1:Spot,2:Average,3:Evaluative,4:Partial,'+
    '5:Center-weighted average';
  rsCanonPanDirLkup = '0:Left to right,1:Right to left,2:Bottom to top,'+
    '3:Top to bottom,4:2x2 Matrix (clockwise)';
  rsCanonQualityLkup = '65535:n/a,1:Economy,2:Normal,3:Fine,4:RAW,5:Superfine,'+
    '130:Normal Movie,131:Movie (2)';
  rsCanonRecLkup = '1:JPEG,2:CRW+THM,3:AVI+THM,4:TIF,5:TIF+JPEG,6:CR2,'+
    '7:CR2+JPEG,9:MOV,10:MP4';
  rsCanonSizeLkup = '65535:n/a,0:Large,1:Medium,2:Small,4:5 MPixel,5:2 MPixel,'+
    '6:1.5 MPixel,8:Postcard,9:Widescreen,10:Medium widescreen,14:Small 1,'+
    '15:Small 2,16:Small 3,128:640x480 movie,129:Medium movie,130:Small movie,'+
    '137:128x720 movie,142:1920x1080 movie';
  rsCanonSloShuttLkup = '65535:n/a,0:Off,1:Night scene,2:On,3:None';
  rsCanonWhiteBalLkup = '0:Auto,1:Daylight,2:Cloudy,3:Tungsten,4:Flourescent,'+
    '5:Flash,6:Custom,7:Black & white,8:Shade,9:Manual temperature (Kelvin),'+
    '14:Daylight fluorescent,17:Under water';
  rsCanonZoomLkup = '0:None,1:2x,2:4x,3:Other';

  // Casio
  rsCasioAFMode2Lkup = '0:Off,1:Spot,2:Multi,3:Face detection,4:Tracking,5:Intelligent';
  rsCasioArtMode2Lkup = '0:Normal,8:Silent movie,39:HDR,45:Premium auto,' +
    '47:Painting,49:Crayon drawing,51:Panorama,52:Art HDR,62:High Speed night shot,'+
    '64:Monochrome,67:Toy camera,68:Pop art,69:Light tone';
  rsCasioAutoIso2Lkup = '1:On,2:Off,7:On (high sensitivity),8:On (anti-shake),'+
    '10:High Speed';
  rsCasioCCDSensitivityLkup = '64:Normal,125:+1.0,250:+2.0,244:+3.0,80:Normal,'+
    '100:High';
  rsCasioColorFilter2Lkup = '0:Off,1:Blue,3:Green,4:Yellow,5:Red,6:Purple,7:Pink';
  rsCasioColorMode2Lkup = '0:Off,2:Black & White,3:Sepia';
  rsCasioDigitalZoomLkup = '$10000:Off,$10001:2x Digital zoom,'+
    '$20000:2x digital zoom,$40000:4x digital zoom';
  rsCasioDriveMode2Lkup = '0:Single shot,1:Continuous shooting,'+
    '2:Continuous (2 fps),3:Continuous (3 fps),4:Continuous (4 fps),'+
    '5:Continuous (5 fps),6:Continuous (6 fps),7:Continuous (7 fps),'+
    '10:Continuous (10 fps),12:Continuous (12 fps),15:Continuous (15 fps),'+
    '20:Continuous (20 fps),30:Continuous (30 fps),40:Continuous (40 fps),'+
    '60:Continuous (60 fps),240:Auto-N';
  rsCasioEnhancement2Lkup = '0:Off,1:Scenery,3:Green,5:Underwater,9:Flesh tones';
  rsCasioFlashIntensityLkup = '11:Weak,13:Normal,15:Strong';
  rsCasioFlashModeLkup = '1:Auto,2:On,3:Off,4:Red-eye reduction';
  rsCasioFocusingModeLkup = '2:Macro,3:Auto focus,4:Manual focus,5:Infinity';
  rsCasioFocusMode2Lkup = '0:Normal,1:Macro';
  rsCasioFocusMode22Lkup = '0:Manual,1:Focus lock,2:Macro,3:Single-area auto focus,'+
    '5:Infinity,6:Multi-area auto focus,8:Super macro';
  rsCasioImageSize2Lkup = '0:640 x 480,4:1600 x 1200,5:2048 x 1536,'+
    '20:2288 x 1712,21:2592 x 1944,22:2304 x 1728,36:3008 x 2008';
  rsCasioImageStabilization2Lkup = '0:Off,1:On,2:Best shot,3:Movie anti-shake';
  rsCasioISOSpeed2Lkup = '3 = 50,4:64,6:100,9:200';
  rsCasioLightingMode2Lkup = '0:Off,1:High dynamic range,5:Shadow enhance low,'+
    '6:Shadow enhance high';
  rsCasioPortraitRefiner2Lkup = '0:Off,1:+1,2:+2';
  rsCasioRecordingModeLkup = '1:Single shutter,2:Panorama,3:Night scene,'+
    '4:Portrait,5:Landscape';
  rsCasioRecordMode2Lkup = '2:Program AE,3:Shutter priority,4:Aperture priority,'+
    '5:Manual,6:Best shot,17:Movie,19:Movie (19),20:YouTube Movie';
  rsCasioReleaseMode2Lkup = '1:Normal,3:AE Bracketing,11:WB Bracketing,'+
    '13 = Contrast Bracketing,19:High Speed Burst';
  rsCasioSharpness2Lkup = '0:Soft,1:Normal,2:Hard';
  rsCasioSpecialEffectSetting2Lkup = '0:Off,1:Makeup,2:Mist removal,'+
    '3:Vivid landscape,16:Art shot';
  rsCasioVideoQuality2Lkup = '1:Standard,3:HD (720p),4:Full HD (1080p),5:Low';
  rsCasioWhiteBalanceLkup = '1:Auto,2:Tungsten,3:Daylight,4:Fluorescent,'+
    '5:Shade,129:Manual';
  rsCasioWhiteBalance2Lkup = '0:Auto,1:Daylight,2:Shade,3:Tungsten,4:Fluorescent,5:Manual';
  rsCasioWhiteBalance22Lkup = '0:Manual,1:Daylight,2:Cloudy,3:Shade,4:Flash?,'+
    '6:Fluorescent,9:Tungsten?,10:Tungsten,12:Flash';

  // Fuji
  rsFujiSharpnessLkup = '0:-4 (softest),1:-3 (very soft),2:-2 (soft),3:0 (normal),' +
    '4:+2 (hard),5:+3 (very hard),6:+4 (hardest),130:-1 (medium soft),'+
    '132:+1 (medium hard),32768:Film Simulation,65535:n/a';
  rsFujiWhiteBalLkup = '0:Auto,256:Daylight,512:Cloudy,768:Daylight Fluorescent,' +
    '769:Day White Fluorescent,770:White Fluorescent,771:Warm White Fluorescent,'+
    '772:Living Room Warm White Fluorescent,1024:Incandescent,1280:Flash,'+
    '1536:Underwater,3840:Custom,3841:Custom2,3842:Custom3,3843:Custom4,'+
    '3844:Custom5,4080:Kelvin';
  rsFujiSaturationLkup = '0:0 (normal),128:+1 (medium high),192:+3 (very high),'+
    '224:+4 (highest),256:+2 (high),384:-1 (medium low),512:Low,768:None (B&W),'+
    '769:B&W Red Filter,770:B&W Yellow Filter,771:B&W Green Filter,'+
    '784:B&W Sepia,1024:-2 (low),1216:-3 (very low),1248:-4 (lowest),'+
    '1280:Acros,1281:Acros Red Filter,1282:Acros Yellow Filter,'+
    '1283:Acros Green Filter,32768:Film Simulation';
  rsFujiContrastLkup = '0:Normal,128:Medium High,256:High,384:Medium Low,'+
    '512:Low,32768:Film Simulation';
  rsFujiContrastLkup1 = '0:Normal,256:High,768:Low';
  rsFujiNoiseReductionLkup = '64:Low,128:Normal,256:n/a';
  rsFujiHighIsoNoiseReductionLkup = '0:0 (normal),256:+2 (strong),'+
    '384:+1 (medium strong),448:+3 (very strong),480:+4 (strongest)'+
    '512:-2 (weak),640:-1 (medium weak),704:-3 (very weak),736:-4 (weakest)';
  rsFujiFlashModeLkup = '0:Auto,1:On,2:Off,3:Red-eye reduction,4:External,'+
    '16:Commander,32768:Not Attached,33056:TTL,38976:Manual,39040:Multi-flash,'+
    '43296:1st Curtain (front),51488:2nd Curtain (rear),59680:High Speed Sync (HSS)';
  rsFujiPictureModeLkup = '0:Auto,1:Portrait,2:Landscape,3:Macro,4:Sports,'+
    '5:Night Scene,6:Program AE,7:Natural Light,8:Anti-blur,9:Beach & Snow,'+
    '10:Sunset,11:Museum,12:Party,13:Flower,14:Text,15:Natural Light & Flash,'+
    '16:Beach,17:Snow,18:Fireworks,19:Underwater,20:Portrait with Skin Correction,'+
    '22:Panorama,23:Night (tripod),24:Pro Low-light,25:Pro Focus,26:Portrait 2,'+
    '27:Dog Face Detection,28:Cat Face Detection,64:Advanced Filter,'+
    '256:Aperture-priority AE,512:Shutter speed priority AE,768:Manual';
  rsFujiEXRModeLkup = '128:HR (High Resolution),512:SN (Signal to Noise priority),'+
    '768:DR (Dynamic Range priority)';
  rsFujiShadowHighlightLkup = '-64:+4 (hardest),-48:+3 (very hard),'+
    '-32:+2 (hard),-16:+1 (medium hard)';
  rsFujiShutterTypeLkup = '0:Mechanical,1:Electronic';
  rsFujiAutoBracketingLkup = '0:Off,1:On,2:No flash & flash';
  rsFujiPanoramaDirLkup = '1:Right,2:Up,3:Left,4:Down';
  rsFujiAdvancedFilterLkup = '65536:Pop Color,131072:Hi Key,196608:Toy Camera,'+
    '262144:Miniature, 327680:Dynamic Tone,327681:Partial Color Red,'+
    '327682:Partial Color Yellow,327683:Partial Color Green,'+
    '327684:Partial Color Blue,327685:Partial Color Orange,'+
    '327686:Partial Color Purple,458752:Soft Focus,589824:Low Key';
  rsFujiColorModeLkup = '0:Standard,16:Chrome,48:B & W';
  rsFujiBlurWarningLkup = '0:None,1:Blur Warning';
  rsFujiFocusWarningLkup = '0:Good,1:Out of focus';
  rsFujiExposureWarningLkup = '0:Good,1:Bad exposure';
  rsFujiDynamicRangeLkup = '1:Standard,3:Wide';
  rsFujiSceneRecognLkup = '0:Unrecognized,256:Portrait Image,512:Landscape Image,'+
    '768:Night Scene,1024:Macro';

  // Minolta
  rsMinoltaBracketStepLkup = '0:1/3 EV,1:2/3 EV,2:1 EV';
  rsMinoltaColorModeLkup = '0:Natural color,1:Black & White,2:Vivid color,'+
    '3:Solarization,4:Adobe RGB,5:Sepia,9:Natural,12:Portrait,13:Natural sRGB,'+
    '14:Natural+ sRGB,15:Landscape,16:Evening,17:Night Scene,18:Night Portrait,'+
    '132:Embed Adobe RGB';
  rsMinoltaColorProfileLkup = '0:Not embedded,1:Embedded';
  rsMinoltaDataImprintLkup = '0;None,1:YYYY/MM/DD,2:MM/DD/HH:MM,3:Text,4:Text + ID#';
  rsMinoltaDECPositionLkup = '0:Exposure,1:Contrast,2:Saturation,3:Filter';
  rsMinoltaDigitalZoomLkup = '0:Off,1:Electronic magnification,2:2x';
  rsMinoltaDriveModeLkup = '0:Single,1:Continuous,2:Self-timer,4:Bracketing,'+
    '5:Interval,6:UHS continuous,7:HS continuous';
  rsMinoltaExposureModeLkup = '0:Program,1:Aperture priority,2:Shutter priority,3:Manual';
  rsMinoltaFocusAreaLkup = '0:Wide Focus (normal),1:Spot Focus';
  rsMinoltaFlashMeteringLkup = '0:ADI (Advanced Distance Integration),1:Pre-flash TTL,2:Manual flash control';
  rsMinoltaFlashModeLkup = '0:Fill flash,1:Red-eye reduction,2:Rear flash sync,3:Wireless,4:Off?';
  rsMinoltaFocusModeLkup = '0:AF,1:MF';
  rsMinoltaFolderNameLkup = '0:Standard Form,1:Data Form';
  rsMinoltaImageSizeLkup = '1:1600x1200,2:1280x960,3:640x480,5:2560x1920,6:2272x1704,7:2048x1536';
  rsMinoltaImageSizeLkup1 = '0:Full,1:1600x1200,2:1280x960,3:640x480,6:2080x1560,7:2560x1920,8;3264x2176';
  rsMinoltaImageStabLkup = '1:Off,5:On';
  rsMinoltaInternalFlashLkup = '0:No,1:Fired';
  rsMinoltaIntervalModeLkup = '0:Still image,1:Time-lapse movie';
  rsMinoltaIsoSettingLkup = '0:100,1:200,2:400,3:800,4:Auto,5:64';
  rsMinoltaMeteringModeLkup = '0:Multi-segment,1:Center-weighted average,2:Spot';
  rsMinoltaModelIDLkup = '0:DiMAGE 7/X1/X21 or X31,1:DiMAGE 5,2:DiMAGE S304,'+
    '3:DiMAGE S404,4:DiMAGE 7i,5:DiMAGE 7Hi,6:DiMAGE A1,7:DiMAGE A2 or S414';
  rsMinoltaQualityLkup = '0:Raw,1:Super Fine,2:Fine,3:Standard,4:Economy,5:Extra fine';
  rsMinoltaSceneModeLkup = '0:Standard,1:Portrait,2:Text,3:Night Scene,'+
    '4:Sunset,5:Sports,6:Landscape,7:Night Portrait,8:Macro,9:Super Macro,'+
    '16:Auto,17:Night View/Portrait,18:Sweep Panorama,19:Handheld Night Shot,'+
    '20:Anti Motion Blur,21:Cont. Priority AE,22:Auto+,23:3D Sweep Panorama,'+
    '24:Superior Auto,25:High Sensitivity,26:Fireworks,27:Food,28:Pet,33:HDR,'+
    '65535:n/a';
  rsMinoltaSharpnessLkup = '0:Hard,1:Normal,2:Soft';
  rsMinoltaSubjectProgramLkup = '0:None,1:Portrait,2:Text,3:Night portrait,4:Sunset,5:Sports action';
  rsMinoltaTeleconverterLkup = '$0:None,$4:Minolta/Sony AF 1.4x APO (D) (0x04),'+
    '$5:Minolta/Sony AF 2x APO (D) (0x05),$48 = Minolta/Sony AF 2x APO (D),'+
    '$50:Minolta AF 2x APO II,$60:Minolta AF 2x APO,$88:Minolta/Sony AF 1.4x APO (D),'+
    '$90 = Minolta AF 1.4x APO II,$A0 = Minolta AF 1.4x APO';
  rsMinoltaWhiteBalanceLkup = '$00:Auto,$01:Color Temperature/Color Filter,$10:Daylight,'+
    '$20:Cloudy,$30:Shade,$40:Tungsten,$50:Flash,$60:Fluorescent,$70:Custom';
  rsMinoltaWideFocusZoneLkup = '0:No zone,1:Center zone (horizontal orientation),'+
    '2:Center zone (vertical orientation),3:Left zone,4:Right zone';
  rsMinoltaZoneMatchingLkup = '0:ISO Setting Used,1:High Key,2:Low Key';

  // Nikon
  rsNikonColorModeLkup = '1:Color,2:Monochrome';
  rsNikonConverterLkup = '0:Not used,1:Used';
  rsNikonImgAdjLkup = '0:Normal,1:Bright+,2:Bright-,3:Contrast+,4:Contrast-';
  rsNikonISOLkup = '0:ISO80,2:ISO160,4:ISO320,5:ISO100';
  rsNikonQualityLkup = '1:Vga Basic,2:Vga Normal,3:Vga Fine,4:SXGA Basic,'+
    '5:SXGA Normal,6:SXGA Fine,10:2 Mpixel Basic,11:2 Mpixel Normal,'+
    '12:2 Mpixel Fine';
  rsNikonWhiteBalanceLkup = '0:Auto,1:Preset,2:Daylight,3:Incandescense,'+
    '4:Fluorescence,5:Cloudy,6:SpeedLight';

  // Olympus
  rsOlympusCCDScanModeLkup = '0:Interlaced,1:Progressive';
  rsOlympusContrastLkup = '0:High,1:Normal,2:Low';
  rsOlympusFlashDevLkup = '0:None,1:Internal,4:External,5:Internal + External';
  rsOlympusFlashModeLkup = '2:On,3;Off';
  rsOlympusFlashModelLkup = '0:None,1:FL-20,2:FL-50,3:RF-11,4:TF-22,5:FL-36,'+
    '6:FL-50R,7:FL-36R,9:FL-14,11:FL-600R';
  rsOlympusFlashTypeLkup = '0:None,2:Simple E-System,3:E-System';
  rsOlympusJpegQualLkup = '1:SQ,2:HQ,3:SHQ,4:Raw';
  rsOlympusMacroLkup = '0:Off,1:On,2:Super Macro';
  rsOlympusPreviewImgLength = 'Preview image length';
  rsOlympusPreviewImgStart = 'Preview image start';
  rsOlympusPreviewImgValid = 'Preview image valid';
  rsOlympusSharpnessLkup = '0:Normal,1:Hard,2:Soft';
  rsOlympusSceneModeLkup = '0:Normal,1:Standard,2:Auto,3:Intelligent Auto,' +
    '4:Portrait,5:Landscape+Portrait,6:Landscape,7:Night Scene,8:Night+Portrait' +
    '9:Sport,10:Self Portrait,11:Indoor,12:Beach & Snow,13:Beach,14:Snow,' +
    '15:Self Portrait+Self Timer,16:Sunset,17:Cuisine,18:Documents,19:Candle,' +
    '20:Fireworks,21:Available Light,22:Vivid,23:Underwater Wide1,24:Underwater Macro,' +
    '25:Museum,26:Behind Glass,27:Auction,28:Shoot & Select1,29:Shoot & Select2,'+
    '30:Underwater Wide2,31:Digital Image Stabilization,32:Face Portrait,33:Pet,'+
    '34:Smile Shot,35:Quick Shutter,43:Hand-held Starlight,100:Panorama,'+
    '101:Magic Filter,103:HDR';

  // Sanyo
  rsSanyoMacroLkup = '0:Normal,1:Macro,2:View,3:Manual';
  rsSanyoQualityLkup = '0:Normal/Very Low,1:Normal/Low,2:Normal/Medium Low,'+
    '3:Normal/Medium,4:Normal/Medium High,5:Normal/High,6:Normal/Very High'+
    '7:Normal/Super High,256:Fine/Very Low,257:Fine/Low,258:Fine/Medium Low'+
    '259:Fine/Medium,260:Fine/Medium High,261:Fine/High,262:Fine/Very High'+
    '263:Fine/Super High,512:Super Fine/Very Low,513:Super Fine/Low,'+
    '514:Super Fine/Medium Low,515:Super Fine/Medium,516:Super Fine/Medium High,'+
    '517:Super Fine/High,518:Super Fine/Very High,519:Super Fine/Super High';
  rsSanyoSpecialMode = 'Special mode';


  // *** IPTC tags ***

  rsActionAdvised = 'Action advised';
  rsByLine = 'ByLine';
  rsByLineTitle = 'ByLine title';
  rsCategory = 'Category';
  rsCity = 'City';
  rsCodedCharSet = 'Coded character set';
  rsContact = 'Contact';
//  rsCopyright = 'Copyright notice';
  rsContentLocCode = 'Content location code';
  rsContentLocName = 'Content location name';
  rsDateCreated = 'Date created';
//  rsDigitizeDate = 'Digital creation date';
//  rsDigitizeTime = 'Digital creation time';
  rsEditorialUpdate = 'Editorial update';
  rsEditStatus = 'Edit status';
  rsExpireDate = 'Expiration date';
  rsExpireTime = 'Expiration time';
  rsFixtureID = 'Fixture ID';
  rsImgCaption = 'Image caption';
  rsImgCaptionWriter = 'Image caption writer';
  rsImgCredit = 'Image credit';
  rsImgHeadline = 'Image headline';
  rsImgType = 'Image type';
  rsIptcOrientationLkup = 'P:Portrait,L:Landscape,S:Square';
  rsKeywords = 'Keywords';
  rsLangID = 'Language ID';
  rsLocationCode = 'Country/primary location code';
  rsLocationName = 'Country/primary location name';
  rsObjectAttr = 'Object attribute reference';
  rsObjectCycle = 'Object cycle';
  rsObjectCycleLkup = 'a:morning,p:evening,b:both';
  rsObjectName = 'Object name';
  rsObjectType = 'Object type reference';
//  rsOrientation = 'Image orientation';
  rsOriginatingProg = 'Originating program';
  rsProgVersion = 'Program version';
  rsRecordVersion = 'Record version';
  rsRefDate = 'Reference date';
  rsRefNumber = 'Reference number';
  rsRefService = 'Reference service';
  rsReleaseDate = 'Release date';
  rsReleaseTime = 'Release time';
  rsSource = 'Source';
  rsSpecialInstruct = 'Special instructions';
  rsState = 'Province/State';
  rsSubjectRef = 'Subject reference';
  rsSubfile = 'Subfile';
  rsSubLocation = 'Sublocation';
  rsSuppCategory = 'Supplemental category';
  rsTimeCreated = 'Time created';
  rsUrgency = 'Urgency';
  rsUrgencyLkup = '0:reserved,1:most urgent,5:normal,8:least urgent,9:reserved';


implementation

end.

