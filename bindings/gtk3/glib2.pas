unit GLib2;

{$MODE OBJFPC}{$H+}

{$PACKRECORDS C}
{$BITPACKING ON}
{$MODESWITCH DUPLICATELOCALS+}

{$LINKLIB libglib-2.0.so.0}
{$LINKLIB libgobject-2.0.so.0}
interface
uses
  CTypes;

const
  GLib2_library = 'libglib-2.0.so.0';

  ALLOCATOR_LIST = 1;
  ALLOCATOR_NODE = 3;
  ALLOCATOR_SLIST = 2;
  ALLOC_AND_FREE = 2;
  ALLOC_ONLY = 1;
  ASCII_DTOSTR_BUF_SIZE = 39;
  ATOMIC_OP_USE_GCC_BUILTINS = 1;
  BIG_ENDIAN = 4321;
  CAN_INLINE = 1;
  CSET_A_2_Z_UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  CSET_DIGITS = '0123456789';
  CSET_a_2_z_lower = 'abcdefghijklmnopqrstuvwxyz';
  DATALIST_FLAGS_MASK = 3;
  DATE_BAD_DAY = 0;
  DATE_BAD_JULIAN = 0;
  DATE_BAD_YEAR = 0;
  DIR_SEPARATOR = 92;
  DIR_SEPARATOR_S = '\';
  E = 2.718282;
  GINT16_FORMAT = 'hi';
  GINT16_MODIFIER = 'h';
  GINT32_FORMAT = 'i';
  GINT32_MODIFIER = '';
  GINT64_FORMAT = 'li';
  GINT64_MODIFIER = 'l';
  GINTPTR_FORMAT = 'li';
  GINTPTR_MODIFIER = 'l';
  GNUC_FUNCTION = '';
  GNUC_PRETTY_FUNCTION = '';
  GSIZE_FORMAT = 'lu';
  GSIZE_MODIFIER = 'l';
  GSSIZE_FORMAT = 'li';
  GUINT16_FORMAT = 'hu';
  GUINT32_FORMAT = 'u';
  GUINT64_FORMAT = 'lu';
  GUINTPTR_FORMAT = 'lu';
  HAVE_GINT64 = 1;
  HAVE_GNUC_VARARGS = 1;
  HAVE_GNUC_VISIBILITY = 1;
  HAVE_GROWING_STACK = 0;
  HAVE_INLINE = 1;
  HAVE_ISO_VARARGS = 1;
  HAVE___INLINE = 1;
  HAVE___INLINE__ = 1;
  HOOK_FLAG_USER_SHIFT = 4;
  IEEE754_DOUBLE_BIAS = 1023;
  IEEE754_FLOAT_BIAS = 127;
  KEY_FILE_DESKTOP_GROUP = 'Desktop Entry';
  KEY_FILE_DESKTOP_KEY_CATEGORIES = 'Categories';
  KEY_FILE_DESKTOP_KEY_COMMENT = 'Comment';
  KEY_FILE_DESKTOP_KEY_EXEC = 'Exec';
  KEY_FILE_DESKTOP_KEY_GENERIC_NAME = 'GenericName';
  KEY_FILE_DESKTOP_KEY_HIDDEN = 'Hidden';
  KEY_FILE_DESKTOP_KEY_ICON = 'Icon';
  KEY_FILE_DESKTOP_KEY_MIME_TYPE = 'MimeType';
  KEY_FILE_DESKTOP_KEY_NAME = 'Name';
  KEY_FILE_DESKTOP_KEY_NOT_SHOW_IN = 'NotShowIn';
  KEY_FILE_DESKTOP_KEY_NO_DISPLAY = 'NoDisplay';
  KEY_FILE_DESKTOP_KEY_ONLY_SHOW_IN = 'OnlyShowIn';
  KEY_FILE_DESKTOP_KEY_PATH = 'Path';
  KEY_FILE_DESKTOP_KEY_STARTUP_NOTIFY = 'StartupNotify';
  KEY_FILE_DESKTOP_KEY_STARTUP_WM_CLASS = 'StartupWMClass';
  KEY_FILE_DESKTOP_KEY_TERMINAL = 'Terminal';
  KEY_FILE_DESKTOP_KEY_TRY_EXEC = 'TryExec';
  KEY_FILE_DESKTOP_KEY_TYPE = 'Type';
  KEY_FILE_DESKTOP_KEY_URL = 'URL';
  KEY_FILE_DESKTOP_KEY_VERSION = 'Version';
  KEY_FILE_DESKTOP_TYPE_APPLICATION = 'Application';
  KEY_FILE_DESKTOP_TYPE_DIRECTORY = 'Directory';
  KEY_FILE_DESKTOP_TYPE_LINK = 'Link';
  LITTLE_ENDIAN = 1234;
  LN10 = 2.302585;
  LN2 = 0.693147;
  LOG_2_BASE_10 = 0.301030;
  LOG_FATAL_MASK = 0;
  LOG_LEVEL_USER_SHIFT = 8;
  MAJOR_VERSION = 2;
  MICRO_VERSION = 18;
  MINOR_VERSION = 29;
  MODULE_SUFFIX = 'so';
  MUTEX_DEBUG_MAGIC = 4175530711;
  OPTION_REMAINING = '';
  PDP_ENDIAN = 3412;
  PI = 3.141593;
  PI_2 = 1.570796;
  PI_4 = 0.785398;
  POLLFD_FORMAT = '%#I64x';
  PRIORITY_DEFAULT = 0;
  PRIORITY_DEFAULT_IDLE = 200;
  PRIORITY_HIGH = -100;
  PRIORITY_HIGH_IDLE = 100;
  PRIORITY_LOW = 300;
  SEARCHPATH_SEPARATOR = 59;
  SEARCHPATH_SEPARATOR_S = ';';
  SIZEOF_LONG = 8;
  SIZEOF_SIZE_T = 8;
  SIZEOF_VOID_P = 8;
  SQRT2 = 1.414214;
  STR_DELIMITERS = '_-|> <.';
  SYSDEF_AF_INET = 2;
  SYSDEF_AF_INET6 = 10;
  SYSDEF_AF_UNIX = 1;
  SYSDEF_MSG_DONTROUTE = 4;
  SYSDEF_MSG_OOB = 1;
  SYSDEF_MSG_PEEK = 2;
  URI_RESERVED_CHARS_GENERIC_DELIMITERS = ':/?#[]@';
  URI_RESERVED_CHARS_SUBCOMPONENT_DELIMITERS = '!$&''()*+,;=';
  USEC_PER_SEC = 1000000;
  VA_COPY_AS_ARRAY = 1;
  WIN32_MSG_HANDLE = 19981206;

  { GAsciiType }
  G_ASCII_ALNUM = 1;
  G_ASCII_ALPHA = 2;
  G_ASCII_CNTRL = 4;
  G_ASCII_DIGIT = 8;
  G_ASCII_GRAPH = 16;
  G_ASCII_LOWER = 32;
  G_ASCII_PRINT = 64;
  G_ASCII_PUNCT = 128;
  G_ASCII_SPACE = 256;
  G_ASCII_UPPER = 512;
  G_ASCII_XDIGIT = 1024;

type
  TGBookmarkFileError = Integer;
const
  { GBookmarkFileError }
  G_BOOKMARK_FILE_ERROR_INVALID_URI: TGBookmarkFileError = 0;
  G_BOOKMARK_FILE_ERROR_INVALID_VALUE: TGBookmarkFileError = 1;
  G_BOOKMARK_FILE_ERROR_APP_NOT_REGISTERED: TGBookmarkFileError = 2;
  G_BOOKMARK_FILE_ERROR_URI_NOT_FOUND: TGBookmarkFileError = 3;
  G_BOOKMARK_FILE_ERROR_READ: TGBookmarkFileError = 4;
  G_BOOKMARK_FILE_ERROR_UNKNOWN_ENCODING: TGBookmarkFileError = 5;
  G_BOOKMARK_FILE_ERROR_WRITE: TGBookmarkFileError = 6;
  G_BOOKMARK_FILE_ERROR_FILE_NOT_FOUND: TGBookmarkFileError = 7;

type
  TGChecksumType = Integer;
const
  { GChecksumType }
  G_CHECKSUM_MD5: TGChecksumType = 0;
  G_CHECKSUM_SHA1: TGChecksumType = 1;
  G_CHECKSUM_SHA256: TGChecksumType = 2;

type
  TGConvertError = Integer;
const
  { GConvertError }
  G_CONVERT_ERROR_NO_CONVERSION: TGConvertError = 0;
  G_CONVERT_ERROR_ILLEGAL_SEQUENCE: TGConvertError = 1;
  G_CONVERT_ERROR_FAILED: TGConvertError = 2;
  G_CONVERT_ERROR_PARTIAL_INPUT: TGConvertError = 3;
  G_CONVERT_ERROR_BAD_URI: TGConvertError = 4;
  G_CONVERT_ERROR_NOT_ABSOLUTE_PATH: TGConvertError = 5;

type
  TGDateMonth = Integer;
const
  { GDateMonth }
  G_DATE_BAD_MONTH: TGDateMonth = 0;
  G_DATE_JANUARY: TGDateMonth = 1;
  G_DATE_FEBRUARY: TGDateMonth = 2;
  G_DATE_MARCH: TGDateMonth = 3;
  G_DATE_APRIL: TGDateMonth = 4;
  G_DATE_MAY: TGDateMonth = 5;
  G_DATE_JUNE: TGDateMonth = 6;
  G_DATE_JULY: TGDateMonth = 7;
  G_DATE_AUGUST: TGDateMonth = 8;
  G_DATE_SEPTEMBER: TGDateMonth = 9;
  G_DATE_OCTOBER: TGDateMonth = 10;
  G_DATE_NOVEMBER: TGDateMonth = 11;
  G_DATE_DECEMBER: TGDateMonth = 12;

type
  TGDateWeekday = Integer;
const
  { GDateWeekday }
  G_DATE_BAD_WEEKDAY: TGDateWeekday = 0;
  G_DATE_MONDAY: TGDateWeekday = 1;
  G_DATE_TUESDAY: TGDateWeekday = 2;
  G_DATE_WEDNESDAY: TGDateWeekday = 3;
  G_DATE_THURSDAY: TGDateWeekday = 4;
  G_DATE_FRIDAY: TGDateWeekday = 5;
  G_DATE_SATURDAY: TGDateWeekday = 6;
  G_DATE_SUNDAY: TGDateWeekday = 7;

type
  TGDateDMY = Integer;
const
  { GDateDMY }
  G_DATE_DAY: TGDateDMY = 0;
  G_DATE_MONTH: TGDateDMY = 1;
  G_DATE_YEAR: TGDateDMY = 2;

type
  TGTimeType = Integer;
const
  { GTimeType }
  G_TIME_TYPE_STANDARD: TGTimeType = 0;
  G_TIME_TYPE_DAYLIGHT: TGTimeType = 1;
  G_TIME_TYPE_UNIVERSAL: TGTimeType = 2;

type
  TGErrorType = Integer;
const
  { GErrorType }
  G_ERR_UNKNOWN: TGErrorType = 0;
  G_ERR_UNEXP_EOF: TGErrorType = 1;
  G_ERR_UNEXP_EOF_IN_STRING: TGErrorType = 2;
  G_ERR_UNEXP_EOF_IN_COMMENT: TGErrorType = 3;
  G_ERR_NON_DIGIT_IN_CONST: TGErrorType = 4;
  G_ERR_DIGIT_RADIX: TGErrorType = 5;
  G_ERR_FLOAT_RADIX: TGErrorType = 6;
  G_ERR_FLOAT_MALFORMED: TGErrorType = 7;

type
  TGFileError = Integer;
const
  { GFileError }
  G_FILE_ERROR_EXIST: TGFileError = 0;
  G_FILE_ERROR_ISDIR: TGFileError = 1;
  G_FILE_ERROR_ACCES: TGFileError = 2;
  G_FILE_ERROR_NAMETOOLONG: TGFileError = 3;
  G_FILE_ERROR_NOENT: TGFileError = 4;
  G_FILE_ERROR_NOTDIR: TGFileError = 5;
  G_FILE_ERROR_NXIO: TGFileError = 6;
  G_FILE_ERROR_NODEV: TGFileError = 7;
  G_FILE_ERROR_ROFS: TGFileError = 8;
  G_FILE_ERROR_TXTBSY: TGFileError = 9;
  G_FILE_ERROR_FAULT: TGFileError = 10;
  G_FILE_ERROR_LOOP: TGFileError = 11;
  G_FILE_ERROR_NOSPC: TGFileError = 12;
  G_FILE_ERROR_NOMEM: TGFileError = 13;
  G_FILE_ERROR_MFILE: TGFileError = 14;
  G_FILE_ERROR_NFILE: TGFileError = 15;
  G_FILE_ERROR_BADF: TGFileError = 16;
  G_FILE_ERROR_INVAL: TGFileError = 17;
  G_FILE_ERROR_PIPE: TGFileError = 18;
  G_FILE_ERROR_AGAIN: TGFileError = 19;
  G_FILE_ERROR_INTR: TGFileError = 20;
  G_FILE_ERROR_IO: TGFileError = 21;
  G_FILE_ERROR_PERM: TGFileError = 22;
  G_FILE_ERROR_NOSYS: TGFileError = 23;
  G_FILE_ERROR_FAILED: TGFileError = 24;

  { GFileTest }
  G_FILE_TEST_IS_REGULAR = 1;
  G_FILE_TEST_IS_SYMLINK = 2;
  G_FILE_TEST_IS_DIR = 4;
  G_FILE_TEST_IS_EXECUTABLE = 8;
  G_FILE_TEST_EXISTS = 16;

  { GFormatSizeFlags }
  G_FORMAT_SIZE_DEFAULT = 0;
  G_FORMAT_SIZE_LONG_FORMAT = 1;
  G_FORMAT_SIZE_IEC_UNITS = 2;

  { GHookFlagMask }
  G_HOOK_FLAG_ACTIVE = 1;
  G_HOOK_FLAG_IN_CALL = 2;
  G_HOOK_FLAG_MASK = 15;

type
  TGSeekType = Integer;
const
  { GSeekType }
  G_SEEK_CUR: TGSeekType = 0;
  G_SEEK_SET: TGSeekType = 1;
  G_SEEK_END: TGSeekType = 2;

  { GIOCondition }
  G_IO_IN = 1;
  G_IO_OUT = 4;
  G_IO_PRI = 2;
  G_IO_ERR = 8;
  G_IO_HUP = 16;
  G_IO_NVAL = 32;

  { GIOFlags }
  G_IO_FLAG_APPEND = 1;
  G_IO_FLAG_NONBLOCK = 2;
  G_IO_FLAG_IS_READABLE = 4;
  G_IO_FLAG_IS_WRITEABLE = 8;
  G_IO_FLAG_IS_SEEKABLE = 16;
  G_IO_FLAG_MASK = 31;
  G_IO_FLAG_GET_MASK = 31;
  G_IO_FLAG_SET_MASK = 3;

type
  TGIOStatus = Integer;
const
  { GIOStatus }
  G_IO_STATUS_ERROR: TGIOStatus = 0;
  G_IO_STATUS_NORMAL: TGIOStatus = 1;
  G_IO_STATUS_EOF: TGIOStatus = 2;
  G_IO_STATUS_AGAIN: TGIOStatus = 3;

type
  TGIOError = Integer;
const
  { GIOError }
  G_IO_ERROR_NONE: TGIOError = 0;
  G_IO_ERROR_AGAIN: TGIOError = 1;
  G_IO_ERROR_INVAL: TGIOError = 2;
  G_IO_ERROR_UNKNOWN: TGIOError = 3;

type
  TGIOChannelError = Integer;
const
  { GIOChannelError }
  G_IO_CHANNEL_ERROR_FBIG: TGIOChannelError = 0;
  G_IO_CHANNEL_ERROR_INVAL: TGIOChannelError = 1;
  G_IO_CHANNEL_ERROR_IO: TGIOChannelError = 2;
  G_IO_CHANNEL_ERROR_ISDIR: TGIOChannelError = 3;
  G_IO_CHANNEL_ERROR_NOSPC: TGIOChannelError = 4;
  G_IO_CHANNEL_ERROR_NXIO: TGIOChannelError = 5;
  G_IO_CHANNEL_ERROR_OVERFLOW: TGIOChannelError = 6;
  G_IO_CHANNEL_ERROR_PIPE: TGIOChannelError = 7;
  G_IO_CHANNEL_ERROR_FAILED: TGIOChannelError = 8;

  { GKeyFileFlags }
  G_KEY_FILE_NONE = 0;
  G_KEY_FILE_KEEP_COMMENTS = 1;
  G_KEY_FILE_KEEP_TRANSLATIONS = 2;

type
  TGKeyFileError = Integer;
const
  { GKeyFileError }
  G_KEY_FILE_ERROR_UNKNOWN_ENCODING: TGKeyFileError = 0;
  G_KEY_FILE_ERROR_PARSE: TGKeyFileError = 1;
  G_KEY_FILE_ERROR_NOT_FOUND: TGKeyFileError = 2;
  G_KEY_FILE_ERROR_KEY_NOT_FOUND: TGKeyFileError = 3;
  G_KEY_FILE_ERROR_GROUP_NOT_FOUND: TGKeyFileError = 4;
  G_KEY_FILE_ERROR_INVALID_VALUE: TGKeyFileError = 5;

  { GLogLevelFlags }
  G_LOG_FLAG_RECURSION = 1;
  G_LOG_FLAG_FATAL = 2;
  G_LOG_LEVEL_ERROR = 4;
  G_LOG_LEVEL_CRITICAL = 8;
  G_LOG_LEVEL_WARNING = 16;
  G_LOG_LEVEL_MESSAGE = 32;
  G_LOG_LEVEL_INFO = 64;
  G_LOG_LEVEL_DEBUG = 128;
  G_LOG_LEVEL_MASK = -4;

  { GMarkupCollectType }
  G_MARKUP_COLLECT_INVALID = 0;
  G_MARKUP_COLLECT_STRING = 1;
  G_MARKUP_COLLECT_STRDUP = 2;
  G_MARKUP_COLLECT_BOOLEAN = 3;
  G_MARKUP_COLLECT_TRISTATE = 4;
  G_MARKUP_COLLECT_OPTIONAL = 65536;

type
  TGMarkupError = Integer;
const
  { GMarkupError }
  G_MARKUP_ERROR_BAD_UTF8: TGMarkupError = 0;
  G_MARKUP_ERROR_EMPTY: TGMarkupError = 1;
  G_MARKUP_ERROR_PARSE: TGMarkupError = 2;
  G_MARKUP_ERROR_UNKNOWN_ELEMENT: TGMarkupError = 3;
  G_MARKUP_ERROR_UNKNOWN_ATTRIBUTE: TGMarkupError = 4;
  G_MARKUP_ERROR_INVALID_CONTENT: TGMarkupError = 5;
  G_MARKUP_ERROR_MISSING_ATTRIBUTE: TGMarkupError = 6;

  { GMarkupParseFlags }
  G_MARKUP_DO_NOT_USE_THIS_UNSUPPORTED_FLAG = 1;
  G_MARKUP_TREAT_CDATA_AS_TEXT = 2;
  G_MARKUP_PREFIX_ERROR_POSITION = 4;

  { GRegexCompileFlags }
  G_REGEX_CASELESS = 1;
  G_REGEX_MULTILINE = 2;
  G_REGEX_DOTALL = 4;
  G_REGEX_EXTENDED = 8;
  G_REGEX_ANCHORED = 16;
  G_REGEX_DOLLAR_ENDONLY = 32;
  G_REGEX_UNGREEDY = 512;
  G_REGEX_RAW = 2048;
  G_REGEX_NO_AUTO_CAPTURE = 4096;
  G_REGEX_OPTIMIZE = 8192;
  G_REGEX_DUPNAMES = 524288;
  G_REGEX_NEWLINE_CR = 1048576;
  G_REGEX_NEWLINE_LF = 2097152;
  G_REGEX_NEWLINE_CRLF = 3145728;

  { GRegexMatchFlags }
  G_REGEX_MATCH_ANCHORED = 16;
  G_REGEX_MATCH_NOTBOL = 128;
  G_REGEX_MATCH_NOTEOL = 256;
  G_REGEX_MATCH_NOTEMPTY = 1024;
  G_REGEX_MATCH_PARTIAL = 32768;
  G_REGEX_MATCH_NEWLINE_CR = 1048576;
  G_REGEX_MATCH_NEWLINE_LF = 2097152;
  G_REGEX_MATCH_NEWLINE_CRLF = 3145728;
  G_REGEX_MATCH_NEWLINE_ANY = 4194304;

  { GTraverseFlags }
  G_TRAVERSE_LEAVES = 1;
  G_TRAVERSE_NON_LEAVES = 2;
  G_TRAVERSE_ALL = 3;
  G_TRAVERSE_MASK = 3;
  G_TRAVERSE_LEAFS = 1;
  G_TRAVERSE_NON_LEAFS = 2;

type
  TGTraverseType = Integer;
const
  { GTraverseType }
  G_IN_ORDER: TGTraverseType = 0;
  G_PRE_ORDER: TGTraverseType = 1;
  G_POST_ORDER: TGTraverseType = 2;
  G_LEVEL_ORDER: TGTraverseType = 3;

type
  TGNormalizeMode = Integer;
const
  { GNormalizeMode }
  G_NORMALIZE_DEFAULT: TGNormalizeMode = 0;
  G_NORMALIZE_NFD: TGNormalizeMode = 0;
  G_NORMALIZE_DEFAULT_COMPOSE: TGNormalizeMode = 1;
  G_NORMALIZE_NFC: TGNormalizeMode = 1;
  G_NORMALIZE_ALL: TGNormalizeMode = 2;
  G_NORMALIZE_NFKD: TGNormalizeMode = 2;
  G_NORMALIZE_ALL_COMPOSE: TGNormalizeMode = 3;
  G_NORMALIZE_NFKC: TGNormalizeMode = 3;

type
  TGOnceStatus = Integer;
const
  { GOnceStatus }
  G_ONCE_STATUS_NOTCALLED: TGOnceStatus = 0;
  G_ONCE_STATUS_PROGRESS: TGOnceStatus = 1;
  G_ONCE_STATUS_READY: TGOnceStatus = 2;

type
  TGOptionArg = Integer;
const
  { GOptionArg }
  G_OPTION_ARG_NONE: TGOptionArg = 0;
  G_OPTION_ARG_STRING: TGOptionArg = 1;
  G_OPTION_ARG_INT: TGOptionArg = 2;
  G_OPTION_ARG_CALLBACK: TGOptionArg = 3;
  G_OPTION_ARG_FILENAME: TGOptionArg = 4;
  G_OPTION_ARG_STRING_ARRAY: TGOptionArg = 5;
  G_OPTION_ARG_FILENAME_ARRAY: TGOptionArg = 6;
  G_OPTION_ARG_DOUBLE: TGOptionArg = 7;
  G_OPTION_ARG_INT64: TGOptionArg = 8;

type
  TGOptionError = Integer;
const
  { GOptionError }
  G_OPTION_ERROR_UNKNOWN_OPTION: TGOptionError = 0;
  G_OPTION_ERROR_BAD_VALUE: TGOptionError = 1;
  G_OPTION_ERROR_FAILED: TGOptionError = 2;

  { GOptionFlags }
  G_OPTION_FLAG_HIDDEN = 1;
  G_OPTION_FLAG_IN_MAIN = 2;
  G_OPTION_FLAG_REVERSE = 4;
  G_OPTION_FLAG_NO_ARG = 8;
  G_OPTION_FLAG_FILENAME = 16;
  G_OPTION_FLAG_OPTIONAL_ARG = 32;
  G_OPTION_FLAG_NOALIAS = 64;

type
  TGRegexError = Integer;
const
  { GRegexError }
  G_REGEX_ERROR_COMPILE: TGRegexError = 0;
  G_REGEX_ERROR_OPTIMIZE: TGRegexError = 1;
  G_REGEX_ERROR_REPLACE: TGRegexError = 2;
  G_REGEX_ERROR_MATCH: TGRegexError = 3;
  G_REGEX_ERROR_INTERNAL: TGRegexError = 4;
  G_REGEX_ERROR_STRAY_BACKSLASH: TGRegexError = 101;
  G_REGEX_ERROR_MISSING_CONTROL_CHAR: TGRegexError = 102;
  G_REGEX_ERROR_UNRECOGNIZED_ESCAPE: TGRegexError = 103;
  G_REGEX_ERROR_QUANTIFIERS_OUT_OF_ORDER: TGRegexError = 104;
  G_REGEX_ERROR_QUANTIFIER_TOO_BIG: TGRegexError = 105;
  G_REGEX_ERROR_UNTERMINATED_CHARACTER_CLASS: TGRegexError = 106;
  G_REGEX_ERROR_INVALID_ESCAPE_IN_CHARACTER_CLASS: TGRegexError = 107;
  G_REGEX_ERROR_RANGE_OUT_OF_ORDER: TGRegexError = 108;
  G_REGEX_ERROR_NOTHING_TO_REPEAT: TGRegexError = 109;
  G_REGEX_ERROR_UNRECOGNIZED_CHARACTER: TGRegexError = 112;
  G_REGEX_ERROR_POSIX_NAMED_CLASS_OUTSIDE_CLASS: TGRegexError = 113;
  G_REGEX_ERROR_UNMATCHED_PARENTHESIS: TGRegexError = 114;
  G_REGEX_ERROR_INEXISTENT_SUBPATTERN_REFERENCE: TGRegexError = 115;
  G_REGEX_ERROR_UNTERMINATED_COMMENT: TGRegexError = 118;
  G_REGEX_ERROR_EXPRESSION_TOO_LARGE: TGRegexError = 120;
  G_REGEX_ERROR_MEMORY_ERROR: TGRegexError = 121;
  G_REGEX_ERROR_VARIABLE_LENGTH_LOOKBEHIND: TGRegexError = 125;
  G_REGEX_ERROR_MALFORMED_CONDITION: TGRegexError = 126;
  G_REGEX_ERROR_TOO_MANY_CONDITIONAL_BRANCHES: TGRegexError = 127;
  G_REGEX_ERROR_ASSERTION_EXPECTED: TGRegexError = 128;
  G_REGEX_ERROR_UNKNOWN_POSIX_CLASS_NAME: TGRegexError = 130;
  G_REGEX_ERROR_POSIX_COLLATING_ELEMENTS_NOT_SUPPORTED: TGRegexError = 131;
  G_REGEX_ERROR_HEX_CODE_TOO_LARGE: TGRegexError = 134;
  G_REGEX_ERROR_INVALID_CONDITION: TGRegexError = 135;
  G_REGEX_ERROR_SINGLE_BYTE_MATCH_IN_LOOKBEHIND: TGRegexError = 136;
  G_REGEX_ERROR_INFINITE_LOOP: TGRegexError = 140;
  G_REGEX_ERROR_MISSING_SUBPATTERN_NAME_TERMINATOR: TGRegexError = 142;
  G_REGEX_ERROR_DUPLICATE_SUBPATTERN_NAME: TGRegexError = 143;
  G_REGEX_ERROR_MALFORMED_PROPERTY: TGRegexError = 146;
  G_REGEX_ERROR_UNKNOWN_PROPERTY: TGRegexError = 147;
  G_REGEX_ERROR_SUBPATTERN_NAME_TOO_LONG: TGRegexError = 148;
  G_REGEX_ERROR_TOO_MANY_SUBPATTERNS: TGRegexError = 149;
  G_REGEX_ERROR_INVALID_OCTAL_VALUE: TGRegexError = 151;
  G_REGEX_ERROR_TOO_MANY_BRANCHES_IN_DEFINE: TGRegexError = 154;
  G_REGEX_ERROR_DEFINE_REPETION: TGRegexError = 155;
  G_REGEX_ERROR_INCONSISTENT_NEWLINE_OPTIONS: TGRegexError = 156;
  G_REGEX_ERROR_MISSING_BACK_REFERENCE: TGRegexError = 157;

type
  TGTokenType = Integer;
const
  { GTokenType }
  G_TOKEN_EOF: TGTokenType = 0;
  G_TOKEN_LEFT_PAREN: TGTokenType = 40;
  G_TOKEN_RIGHT_PAREN: TGTokenType = 41;
  G_TOKEN_LEFT_CURLY: TGTokenType = 123;
  G_TOKEN_RIGHT_CURLY: TGTokenType = 125;
  G_TOKEN_LEFT_BRACE: TGTokenType = 91;
  G_TOKEN_RIGHT_BRACE: TGTokenType = 93;
  G_TOKEN_EQUAL_SIGN: TGTokenType = 61;
  G_TOKEN_COMMA: TGTokenType = 44;
  G_TOKEN_NONE: TGTokenType = 256;
  G_TOKEN_ERROR: TGTokenType = 257;
  G_TOKEN_CHAR: TGTokenType = 258;
  G_TOKEN_BINARY: TGTokenType = 259;
  G_TOKEN_OCTAL: TGTokenType = 260;
  G_TOKEN_INT: TGTokenType = 261;
  G_TOKEN_HEX: TGTokenType = 262;
  G_TOKEN_FLOAT: TGTokenType = 263;
  G_TOKEN_STRING: TGTokenType = 264;
  G_TOKEN_SYMBOL: TGTokenType = 265;
  G_TOKEN_IDENTIFIER: TGTokenType = 266;
  G_TOKEN_IDENTIFIER_NULL: TGTokenType = 267;
  G_TOKEN_COMMENT_SINGLE: TGTokenType = 268;
  G_TOKEN_COMMENT_MULTI: TGTokenType = 269;
  G_TOKEN_LAST: TGTokenType = 270;

type
  TGShellError = Integer;
const
  { GShellError }
  G_SHELL_ERROR_BAD_QUOTING: TGShellError = 0;
  G_SHELL_ERROR_EMPTY_STRING: TGShellError = 1;
  G_SHELL_ERROR_FAILED: TGShellError = 2;

type
  TGSliceConfig = Integer;
const
  { GSliceConfig }
  G_SLICE_CONFIG_ALWAYS_MALLOC: TGSliceConfig = 1;
  G_SLICE_CONFIG_BYPASS_MAGAZINES: TGSliceConfig = 2;
  G_SLICE_CONFIG_WORKING_SET_MSECS: TGSliceConfig = 3;
  G_SLICE_CONFIG_COLOR_INCREMENT: TGSliceConfig = 4;
  G_SLICE_CONFIG_CHUNK_SIZES: TGSliceConfig = 5;
  G_SLICE_CONFIG_CONTENTION_COUNTER: TGSliceConfig = 6;

type
  TGSpawnError = Integer;
const
  { GSpawnError }
  G_SPAWN_ERROR_FORK: TGSpawnError = 0;
  G_SPAWN_ERROR_READ: TGSpawnError = 1;
  G_SPAWN_ERROR_CHDIR: TGSpawnError = 2;
  G_SPAWN_ERROR_ACCES: TGSpawnError = 3;
  G_SPAWN_ERROR_PERM: TGSpawnError = 4;
  G_SPAWN_ERROR_2BIG: TGSpawnError = 5;
  G_SPAWN_ERROR_NOEXEC: TGSpawnError = 6;
  G_SPAWN_ERROR_NAMETOOLONG: TGSpawnError = 7;
  G_SPAWN_ERROR_NOENT: TGSpawnError = 8;
  G_SPAWN_ERROR_NOMEM: TGSpawnError = 9;
  G_SPAWN_ERROR_NOTDIR: TGSpawnError = 10;
  G_SPAWN_ERROR_LOOP: TGSpawnError = 11;
  G_SPAWN_ERROR_TXTBUSY: TGSpawnError = 12;
  G_SPAWN_ERROR_IO: TGSpawnError = 13;
  G_SPAWN_ERROR_NFILE: TGSpawnError = 14;
  G_SPAWN_ERROR_MFILE: TGSpawnError = 15;
  G_SPAWN_ERROR_INVAL: TGSpawnError = 16;
  G_SPAWN_ERROR_ISDIR: TGSpawnError = 17;
  G_SPAWN_ERROR_LIBBAD: TGSpawnError = 18;
  G_SPAWN_ERROR_FAILED: TGSpawnError = 19;

  { GSpawnFlags }
  G_SPAWN_LEAVE_DESCRIPTORS_OPEN = 1;
  G_SPAWN_DO_NOT_REAP_CHILD = 2;
  G_SPAWN_SEARCH_PATH = 4;
  G_SPAWN_STDOUT_TO_DEV_NULL = 8;
  G_SPAWN_STDERR_TO_DEV_NULL = 16;
  G_SPAWN_CHILD_INHERITS_STDIN = 32;
  G_SPAWN_FILE_AND_ARGV_ZERO = 64;

type
  TGTestLogType = Integer;
const
  { GTestLogType }
  G_TEST_LOG_NONE: TGTestLogType = 0;
  G_TEST_LOG_ERROR: TGTestLogType = 1;
  G_TEST_LOG_START_BINARY: TGTestLogType = 2;
  G_TEST_LOG_LIST_CASE: TGTestLogType = 3;
  G_TEST_LOG_SKIP_CASE: TGTestLogType = 4;
  G_TEST_LOG_START_CASE: TGTestLogType = 5;
  G_TEST_LOG_STOP_CASE: TGTestLogType = 6;
  G_TEST_LOG_MIN_RESULT: TGTestLogType = 7;
  G_TEST_LOG_MAX_RESULT: TGTestLogType = 8;
  G_TEST_LOG_MESSAGE: TGTestLogType = 9;

  { GTestTrapFlags }
  G_TEST_TRAP_SILENCE_STDOUT = 128;
  G_TEST_TRAP_SILENCE_STDERR = 256;
  G_TEST_TRAP_INHERIT_STDIN = 512;

type
  TGThreadPriority = Integer;
const
  { GThreadPriority }
  G_THREAD_PRIORITY_LOW: TGThreadPriority = 0;
  G_THREAD_PRIORITY_NORMAL: TGThreadPriority = 1;
  G_THREAD_PRIORITY_HIGH: TGThreadPriority = 2;
  G_THREAD_PRIORITY_URGENT: TGThreadPriority = 3;

type
  TGThreadError = Integer;
const
  { GThreadError }
  G_THREAD_ERROR_AGAIN: TGThreadError = 0;

type
  TGUnicodeBreakType = Integer;
const
  { GUnicodeBreakType }
  G_UNICODE_BREAK_MANDATORY: TGUnicodeBreakType = 0;
  G_UNICODE_BREAK_CARRIAGE_RETURN: TGUnicodeBreakType = 1;
  G_UNICODE_BREAK_LINE_FEED: TGUnicodeBreakType = 2;
  G_UNICODE_BREAK_COMBINING_MARK: TGUnicodeBreakType = 3;
  G_UNICODE_BREAK_SURROGATE: TGUnicodeBreakType = 4;
  G_UNICODE_BREAK_ZERO_WIDTH_SPACE: TGUnicodeBreakType = 5;
  G_UNICODE_BREAK_INSEPARABLE: TGUnicodeBreakType = 6;
  G_UNICODE_BREAK_NON_BREAKING_GLUE: TGUnicodeBreakType = 7;
  G_UNICODE_BREAK_CONTINGENT: TGUnicodeBreakType = 8;
  G_UNICODE_BREAK_SPACE: TGUnicodeBreakType = 9;
  G_UNICODE_BREAK_AFTER: TGUnicodeBreakType = 10;
  G_UNICODE_BREAK_BEFORE: TGUnicodeBreakType = 11;
  G_UNICODE_BREAK_BEFORE_AND_AFTER: TGUnicodeBreakType = 12;
  G_UNICODE_BREAK_HYPHEN: TGUnicodeBreakType = 13;
  G_UNICODE_BREAK_NON_STARTER: TGUnicodeBreakType = 14;
  G_UNICODE_BREAK_OPEN_PUNCTUATION: TGUnicodeBreakType = 15;
  G_UNICODE_BREAK_CLOSE_PUNCTUATION: TGUnicodeBreakType = 16;
  G_UNICODE_BREAK_QUOTATION: TGUnicodeBreakType = 17;
  G_UNICODE_BREAK_EXCLAMATION: TGUnicodeBreakType = 18;
  G_UNICODE_BREAK_IDEOGRAPHIC: TGUnicodeBreakType = 19;
  G_UNICODE_BREAK_NUMERIC: TGUnicodeBreakType = 20;
  G_UNICODE_BREAK_INFIX_SEPARATOR: TGUnicodeBreakType = 21;
  G_UNICODE_BREAK_SYMBOL: TGUnicodeBreakType = 22;
  G_UNICODE_BREAK_ALPHABETIC: TGUnicodeBreakType = 23;
  G_UNICODE_BREAK_PREFIX: TGUnicodeBreakType = 24;
  G_UNICODE_BREAK_POSTFIX: TGUnicodeBreakType = 25;
  G_UNICODE_BREAK_COMPLEX_CONTEXT: TGUnicodeBreakType = 26;
  G_UNICODE_BREAK_AMBIGUOUS: TGUnicodeBreakType = 27;
  G_UNICODE_BREAK_UNKNOWN: TGUnicodeBreakType = 28;
  G_UNICODE_BREAK_NEXT_LINE: TGUnicodeBreakType = 29;
  G_UNICODE_BREAK_WORD_JOINER: TGUnicodeBreakType = 30;
  G_UNICODE_BREAK_HANGUL_L_JAMO: TGUnicodeBreakType = 31;
  G_UNICODE_BREAK_HANGUL_V_JAMO: TGUnicodeBreakType = 32;
  G_UNICODE_BREAK_HANGUL_T_JAMO: TGUnicodeBreakType = 33;
  G_UNICODE_BREAK_HANGUL_LV_SYLLABLE: TGUnicodeBreakType = 34;
  G_UNICODE_BREAK_HANGUL_LVT_SYLLABLE: TGUnicodeBreakType = 35;
  G_UNICODE_BREAK_CLOSE_PARANTHESIS: TGUnicodeBreakType = 36;

type
  TGUnicodeScript = Integer;
const
  { GUnicodeScript }
  G_UNICODE_SCRIPT_INVALID_CODE: TGUnicodeScript = -1;
  G_UNICODE_SCRIPT_COMMON: TGUnicodeScript = 0;
  G_UNICODE_SCRIPT_INHERITED: TGUnicodeScript = 1;
  G_UNICODE_SCRIPT_ARABIC: TGUnicodeScript = 2;
  G_UNICODE_SCRIPT_ARMENIAN: TGUnicodeScript = 3;
  G_UNICODE_SCRIPT_BENGALI: TGUnicodeScript = 4;
  G_UNICODE_SCRIPT_BOPOMOFO: TGUnicodeScript = 5;
  G_UNICODE_SCRIPT_CHEROKEE: TGUnicodeScript = 6;
  G_UNICODE_SCRIPT_COPTIC: TGUnicodeScript = 7;
  G_UNICODE_SCRIPT_CYRILLIC: TGUnicodeScript = 8;
  G_UNICODE_SCRIPT_DESERET: TGUnicodeScript = 9;
  G_UNICODE_SCRIPT_DEVANAGARI: TGUnicodeScript = 10;
  G_UNICODE_SCRIPT_ETHIOPIC: TGUnicodeScript = 11;
  G_UNICODE_SCRIPT_GEORGIAN: TGUnicodeScript = 12;
  G_UNICODE_SCRIPT_GOTHIC: TGUnicodeScript = 13;
  G_UNICODE_SCRIPT_GREEK: TGUnicodeScript = 14;
  G_UNICODE_SCRIPT_GUJARATI: TGUnicodeScript = 15;
  G_UNICODE_SCRIPT_GURMUKHI: TGUnicodeScript = 16;
  G_UNICODE_SCRIPT_HAN: TGUnicodeScript = 17;
  G_UNICODE_SCRIPT_HANGUL: TGUnicodeScript = 18;
  G_UNICODE_SCRIPT_HEBREW: TGUnicodeScript = 19;
  G_UNICODE_SCRIPT_HIRAGANA: TGUnicodeScript = 20;
  G_UNICODE_SCRIPT_KANNADA: TGUnicodeScript = 21;
  G_UNICODE_SCRIPT_KATAKANA: TGUnicodeScript = 22;
  G_UNICODE_SCRIPT_KHMER: TGUnicodeScript = 23;
  G_UNICODE_SCRIPT_LAO: TGUnicodeScript = 24;
  G_UNICODE_SCRIPT_LATIN: TGUnicodeScript = 25;
  G_UNICODE_SCRIPT_MALAYALAM: TGUnicodeScript = 26;
  G_UNICODE_SCRIPT_MONGOLIAN: TGUnicodeScript = 27;
  G_UNICODE_SCRIPT_MYANMAR: TGUnicodeScript = 28;
  G_UNICODE_SCRIPT_OGHAM: TGUnicodeScript = 29;
  G_UNICODE_SCRIPT_OLD_ITALIC: TGUnicodeScript = 30;
  G_UNICODE_SCRIPT_ORIYA: TGUnicodeScript = 31;
  G_UNICODE_SCRIPT_RUNIC: TGUnicodeScript = 32;
  G_UNICODE_SCRIPT_SINHALA: TGUnicodeScript = 33;
  G_UNICODE_SCRIPT_SYRIAC: TGUnicodeScript = 34;
  G_UNICODE_SCRIPT_TAMIL: TGUnicodeScript = 35;
  G_UNICODE_SCRIPT_TELUGU: TGUnicodeScript = 36;
  G_UNICODE_SCRIPT_THAANA: TGUnicodeScript = 37;
  G_UNICODE_SCRIPT_THAI: TGUnicodeScript = 38;
  G_UNICODE_SCRIPT_TIBETAN: TGUnicodeScript = 39;
  G_UNICODE_SCRIPT_CANADIAN_ABORIGINAL: TGUnicodeScript = 40;
  G_UNICODE_SCRIPT_YI: TGUnicodeScript = 41;
  G_UNICODE_SCRIPT_TAGALOG: TGUnicodeScript = 42;
  G_UNICODE_SCRIPT_HANUNOO: TGUnicodeScript = 43;
  G_UNICODE_SCRIPT_BUHID: TGUnicodeScript = 44;
  G_UNICODE_SCRIPT_TAGBANWA: TGUnicodeScript = 45;
  G_UNICODE_SCRIPT_BRAILLE: TGUnicodeScript = 46;
  G_UNICODE_SCRIPT_CYPRIOT: TGUnicodeScript = 47;
  G_UNICODE_SCRIPT_LIMBU: TGUnicodeScript = 48;
  G_UNICODE_SCRIPT_OSMANYA: TGUnicodeScript = 49;
  G_UNICODE_SCRIPT_SHAVIAN: TGUnicodeScript = 50;
  G_UNICODE_SCRIPT_LINEAR_B: TGUnicodeScript = 51;
  G_UNICODE_SCRIPT_TAI_LE: TGUnicodeScript = 52;
  G_UNICODE_SCRIPT_UGARITIC: TGUnicodeScript = 53;
  G_UNICODE_SCRIPT_NEW_TAI_LUE: TGUnicodeScript = 54;
  G_UNICODE_SCRIPT_BUGINESE: TGUnicodeScript = 55;
  G_UNICODE_SCRIPT_GLAGOLITIC: TGUnicodeScript = 56;
  G_UNICODE_SCRIPT_TIFINAGH: TGUnicodeScript = 57;
  G_UNICODE_SCRIPT_SYLOTI_NAGRI: TGUnicodeScript = 58;
  G_UNICODE_SCRIPT_OLD_PERSIAN: TGUnicodeScript = 59;
  G_UNICODE_SCRIPT_KHAROSHTHI: TGUnicodeScript = 60;
  G_UNICODE_SCRIPT_UNKNOWN: TGUnicodeScript = 61;
  G_UNICODE_SCRIPT_BALINESE: TGUnicodeScript = 62;
  G_UNICODE_SCRIPT_CUNEIFORM: TGUnicodeScript = 63;
  G_UNICODE_SCRIPT_PHOENICIAN: TGUnicodeScript = 64;
  G_UNICODE_SCRIPT_PHAGS_PA: TGUnicodeScript = 65;
  G_UNICODE_SCRIPT_NKO: TGUnicodeScript = 66;
  G_UNICODE_SCRIPT_KAYAH_LI: TGUnicodeScript = 67;
  G_UNICODE_SCRIPT_LEPCHA: TGUnicodeScript = 68;
  G_UNICODE_SCRIPT_REJANG: TGUnicodeScript = 69;
  G_UNICODE_SCRIPT_SUNDANESE: TGUnicodeScript = 70;
  G_UNICODE_SCRIPT_SAURASHTRA: TGUnicodeScript = 71;
  G_UNICODE_SCRIPT_CHAM: TGUnicodeScript = 72;
  G_UNICODE_SCRIPT_OL_CHIKI: TGUnicodeScript = 73;
  G_UNICODE_SCRIPT_VAI: TGUnicodeScript = 74;
  G_UNICODE_SCRIPT_CARIAN: TGUnicodeScript = 75;
  G_UNICODE_SCRIPT_LYCIAN: TGUnicodeScript = 76;
  G_UNICODE_SCRIPT_LYDIAN: TGUnicodeScript = 77;
  G_UNICODE_SCRIPT_AVESTAN: TGUnicodeScript = 78;
  G_UNICODE_SCRIPT_BAMUM: TGUnicodeScript = 79;
  G_UNICODE_SCRIPT_EGYPTIAN_HIEROGLYPHS: TGUnicodeScript = 80;
  G_UNICODE_SCRIPT_IMPERIAL_ARAMAIC: TGUnicodeScript = 81;
  G_UNICODE_SCRIPT_INSCRIPTIONAL_PAHLAVI: TGUnicodeScript = 82;
  G_UNICODE_SCRIPT_INSCRIPTIONAL_PARTHIAN: TGUnicodeScript = 83;
  G_UNICODE_SCRIPT_JAVANESE: TGUnicodeScript = 84;
  G_UNICODE_SCRIPT_KAITHI: TGUnicodeScript = 85;
  G_UNICODE_SCRIPT_LISU: TGUnicodeScript = 86;
  G_UNICODE_SCRIPT_MEETEI_MAYEK: TGUnicodeScript = 87;
  G_UNICODE_SCRIPT_OLD_SOUTH_ARABIAN: TGUnicodeScript = 88;
  G_UNICODE_SCRIPT_OLD_TURKIC: TGUnicodeScript = 89;
  G_UNICODE_SCRIPT_SAMARITAN: TGUnicodeScript = 90;
  G_UNICODE_SCRIPT_TAI_THAM: TGUnicodeScript = 91;
  G_UNICODE_SCRIPT_TAI_VIET: TGUnicodeScript = 92;
  G_UNICODE_SCRIPT_BATAK: TGUnicodeScript = 93;
  G_UNICODE_SCRIPT_BRAHMI: TGUnicodeScript = 94;
  G_UNICODE_SCRIPT_MANDAIC: TGUnicodeScript = 95;

type
  TGUnicodeType = Integer;
const
  { GUnicodeType }
  G_UNICODE_CONTROL: TGUnicodeType = 0;
  G_UNICODE_FORMAT: TGUnicodeType = 1;
  G_UNICODE_UNASSIGNED: TGUnicodeType = 2;
  G_UNICODE_PRIVATE_USE: TGUnicodeType = 3;
  G_UNICODE_SURROGATE: TGUnicodeType = 4;
  G_UNICODE_LOWERCASE_LETTER: TGUnicodeType = 5;
  G_UNICODE_MODIFIER_LETTER: TGUnicodeType = 6;
  G_UNICODE_OTHER_LETTER: TGUnicodeType = 7;
  G_UNICODE_TITLECASE_LETTER: TGUnicodeType = 8;
  G_UNICODE_UPPERCASE_LETTER: TGUnicodeType = 9;
  G_UNICODE_SPACING_MARK: TGUnicodeType = 10;
  G_UNICODE_ENCLOSING_MARK: TGUnicodeType = 11;
  G_UNICODE_NON_SPACING_MARK: TGUnicodeType = 12;
  G_UNICODE_DECIMAL_NUMBER: TGUnicodeType = 13;
  G_UNICODE_LETTER_NUMBER: TGUnicodeType = 14;
  G_UNICODE_OTHER_NUMBER: TGUnicodeType = 15;
  G_UNICODE_CONNECT_PUNCTUATION: TGUnicodeType = 16;
  G_UNICODE_DASH_PUNCTUATION: TGUnicodeType = 17;
  G_UNICODE_CLOSE_PUNCTUATION: TGUnicodeType = 18;
  G_UNICODE_FINAL_PUNCTUATION: TGUnicodeType = 19;
  G_UNICODE_INITIAL_PUNCTUATION: TGUnicodeType = 20;
  G_UNICODE_OTHER_PUNCTUATION: TGUnicodeType = 21;
  G_UNICODE_OPEN_PUNCTUATION: TGUnicodeType = 22;
  G_UNICODE_CURRENCY_SYMBOL: TGUnicodeType = 23;
  G_UNICODE_MODIFIER_SYMBOL: TGUnicodeType = 24;
  G_UNICODE_MATH_SYMBOL: TGUnicodeType = 25;
  G_UNICODE_OTHER_SYMBOL: TGUnicodeType = 26;
  G_UNICODE_LINE_SEPARATOR: TGUnicodeType = 27;
  G_UNICODE_PARAGRAPH_SEPARATOR: TGUnicodeType = 28;
  G_UNICODE_SPACE_SEPARATOR: TGUnicodeType = 29;

type
  TGUserDirectory = Integer;
const
  { GUserDirectory }
  G_USER_DIRECTORY_DESKTOP: TGUserDirectory = 0;
  G_USER_DIRECTORY_DOCUMENTS: TGUserDirectory = 1;
  G_USER_DIRECTORY_DOWNLOAD: TGUserDirectory = 2;
  G_USER_DIRECTORY_MUSIC: TGUserDirectory = 3;
  G_USER_DIRECTORY_PICTURES: TGUserDirectory = 4;
  G_USER_DIRECTORY_PUBLIC_SHARE: TGUserDirectory = 5;
  G_USER_DIRECTORY_TEMPLATES: TGUserDirectory = 6;
  G_USER_DIRECTORY_VIDEOS: TGUserDirectory = 7;
  G_USER_N_DIRECTORIES: TGUserDirectory = 8;

type
  TGVariantClass = Integer;
const
  { GVariantClass }
  G_VARIANT_CLASS_BOOLEAN: TGVariantClass = 98;
  G_VARIANT_CLASS_BYTE: TGVariantClass = 121;
  G_VARIANT_CLASS_INT16: TGVariantClass = 110;
  G_VARIANT_CLASS_UINT16: TGVariantClass = 113;
  G_VARIANT_CLASS_INT32: TGVariantClass = 105;
  G_VARIANT_CLASS_UINT32: TGVariantClass = 117;
  G_VARIANT_CLASS_INT64: TGVariantClass = 120;
  G_VARIANT_CLASS_UINT64: TGVariantClass = 116;
  G_VARIANT_CLASS_HANDLE: TGVariantClass = 104;
  G_VARIANT_CLASS_DOUBLE: TGVariantClass = 100;
  G_VARIANT_CLASS_STRING: TGVariantClass = 115;
  G_VARIANT_CLASS_OBJECT_PATH: TGVariantClass = 111;
  G_VARIANT_CLASS_SIGNATURE: TGVariantClass = 103;
  G_VARIANT_CLASS_VARIANT: TGVariantClass = 118;
  G_VARIANT_CLASS_MAYBE: TGVariantClass = 109;
  G_VARIANT_CLASS_ARRAY: TGVariantClass = 97;
  G_VARIANT_CLASS_TUPLE: TGVariantClass = 40;
  G_VARIANT_CLASS_DICT_ENTRY: TGVariantClass = 123;

type
  TGVariantParseError = Integer;
const
  { GVariantParseError }
  G_VARIANT_PARSE_ERROR_FAILED: TGVariantParseError = 0;
  G_VARIANT_PARSE_ERROR_BASIC_TYPE_EXPECTED: TGVariantParseError = 1;
  G_VARIANT_PARSE_ERROR_CANNOT_INFER_TYPE: TGVariantParseError = 2;
  G_VARIANT_PARSE_ERROR_DEFINITE_TYPE_EXPECTED: TGVariantParseError = 3;
  G_VARIANT_PARSE_ERROR_INPUT_NOT_AT_END: TGVariantParseError = 4;
  G_VARIANT_PARSE_ERROR_INVALID_CHARACTER: TGVariantParseError = 5;
  G_VARIANT_PARSE_ERROR_INVALID_FORMAT_STRING: TGVariantParseError = 6;
  G_VARIANT_PARSE_ERROR_INVALID_OBJECT_PATH: TGVariantParseError = 7;
  G_VARIANT_PARSE_ERROR_INVALID_SIGNATURE: TGVariantParseError = 8;
  G_VARIANT_PARSE_ERROR_INVALID_TYPE_STRING: TGVariantParseError = 9;
  G_VARIANT_PARSE_ERROR_NO_COMMON_TYPE: TGVariantParseError = 10;
  G_VARIANT_PARSE_ERROR_NUMBER_OUT_OF_RANGE: TGVariantParseError = 11;
  G_VARIANT_PARSE_ERROR_NUMBER_TOO_BIG: TGVariantParseError = 12;
  G_VARIANT_PARSE_ERROR_TYPE_ERROR: TGVariantParseError = 13;
  G_VARIANT_PARSE_ERROR_UNEXPECTED_TOKEN: TGVariantParseError = 14;
  G_VARIANT_PARSE_ERROR_UNKNOWN_KEYWORD: TGVariantParseError = 15;
  G_VARIANT_PARSE_ERROR_UNTERMINATED_STRING_CONSTANT: TGVariantParseError = 16;
  G_VARIANT_PARSE_ERROR_VALUE_EXPECTED: TGVariantParseError = 17;
type
  TBitObject32 = object
  protected
    procedure SetBit(AMask: Integer; AValue: DWord);
    function GetBit(AMask: Integer): DWord;
  public
    Flags0: DWord;
    procedure Init(AFlags: DWord);
  end;
  guint1 = 0..(1 shl 1-1);
  guint2 = 0..(1 shl 2-1);
  guint3 = 0..(1 shl 3-1);
  guint4 = 0..(1 shl 4-1);
  guint5 = 0..(1 shl 5-1);
  guint6 = 0..(1 shl 6-1);
  guint7 = 0..(1 shl 7-1);
  guint9 = 0..(1 shl 9-1);
  guint10 = 0..(1 shl 10-1);
  guint11 = 0..(1 shl 11-1);
  guint12 = 0..(1 shl 12-1);
  guint13 = 0..(1 shl 13-1);
  guint14 = 0..(1 shl 14-1);
  guint15 = 0..(1 shl 15-1);
  guint17 = 0..(1 shl 17-1);
  guint18 = 0..(1 shl 18-1);
  guint19 = 0..(1 shl 19-1);
  guint20 = 0..(1 shl 20-1);
  guint21 = 0..(1 shl 21-1);
  guint22 = 0..(1 shl 22-1);
  guint23 = 0..(1 shl 23-1);
  guint24 = 0..(1 shl 24-1);
  guint25 = 0..(1 shl 25-1);
  guint26 = 0..(1 shl 26-1);
  guint27 = 0..(1 shl 27-1);
  guint28 = 0..(1 shl 28-1);
  guint29 = 0..(1 shl 29-1);
  guint30 = 0..(1 shl 30-1);
  guint31 = 0..(1 shl 31-1);
  gpointer = pointer;
  int = cint;
  gint = cint;
  guint = cuint;
  guint8 = cuint8;
  guint16 = cuint16;
  guint32 = cuint32;
  guint64 = cuint64;
  gint8 = cint8;
  gint16 = cint16;
  gint32 = cint32;
  gint64 = cint64;
  gsize = csize_t;
  glong = clong;
  gulong = culong;
  gushort = cushort;
  gchar = char;
  gboolean = Boolean32;
  gssize = PtrInt;
  size_t = csize_t;
  gconstpointer = gpointer;
  gfloat = cfloat;
  gdouble = cdouble;
  double = cdouble;
  goffset = Int64;
  long_double = Extended;
  gunichar = guint32;
  gunichar2 = guint32;
  unsigned_long_long = qword;

  PPGDateDay = ^PGDateDay;
  PGDateDay = ^TGDateDay;
  TGDateDay = guint8;

  PPGDateYear = ^PGDateYear;
  PGDateYear = ^TGDateYear;
  TGDateYear = guint16;

  PPGPid = ^PGPid;
  PGPid = ^TGPid;
  TGPid = gint;

  PPGQuark = ^PGQuark;
  PGQuark = ^TGQuark;
  TGQuark = guint32;

  PPGStrv = ^PGStrv;
  PGStrv = ^TGStrv;
  TGStrv = gpointer;

  PPGTime = ^PGTime;
  PGTime = ^TGTime;
  TGTime = gint32;

  PPGTimeSpan = ^PGTimeSpan;
  PGTimeSpan = ^TGTimeSpan;
  TGTimeSpan = gint64;

  PPGType = ^PGType;
  PGType = ^TGType;
  TGType = gsize;

  { void }
  Tvoid = record
    { opaque type }
    Unknown: Pointer;
  end;


  PPGAllocator = ^PGAllocator;
  PGAllocator = ^TGAllocator;

  PPPgchar = ^PPgchar;
  PPgchar = ^Pgchar;
  Pgchar = ^gchar;

  PPPguint = ^PPguint;
  PPguint = ^Pguint;
  Pguint = ^guint;
  TGAllocator = object
    procedure free; cdecl; inline;
    function new(name: Pgchar; n_preallocs: guint): PGAllocator; cdecl; inline; static;
  end;

  PPPPPgint = ^PPPPgint;
  PPPPgint = ^PPPgint;
  PPPgint = ^PPgint;
  PPgint = ^Pgint;
  Pgint = ^gint;

  PPgpointer = ^Pgpointer;
  Pgpointer = ^gpointer;
  TGCompareFunc = function(a: gpointer; b: gpointer): gint; cdecl;
  TGCompareDataFunc = function(a: gpointer; b: gpointer; user_data: gpointer): gint; cdecl;

  PPGArray = ^PGArray;
  PGArray = ^TGArray;

  PPgboolean = ^Pgboolean;
  Pgboolean = ^gboolean;

  PPGCompareFunc = ^PGCompareFunc;
  PGCompareFunc = ^TGCompareFunc;

  PPGCompareDataFunc = ^PGCompareDataFunc;
  PGCompareDataFunc = ^TGCompareDataFunc;
  TGArray = object
    data: Pgchar;
    len: guint;
    function append_vals(array_: Pgpointer; data: gpointer; len: guint): Pgpointer; cdecl; inline; static;
    function free(array_: Pgpointer; free_segment: gboolean): Pgchar; cdecl; inline; static;
    function get_element_size(array_: Pgpointer): guint; cdecl; inline; static;
    function insert_vals(array_: Pgpointer; index_: guint; data: gpointer; len: guint): Pgpointer; cdecl; inline; static;
    function new(zero_terminated: gboolean; clear_: gboolean; element_size: guint): Pgpointer; cdecl; inline; static;
    function prepend_vals(array_: Pgpointer; data: gpointer; len: guint): Pgpointer; cdecl; inline; static;
    function ref(array_: Pgpointer): Pgpointer; cdecl; inline; static;
    function remove_index(array_: Pgpointer; index_: guint): Pgpointer; cdecl; inline; static;
    function remove_index_fast(array_: Pgpointer; index_: guint): Pgpointer; cdecl; inline; static;
    function remove_range(array_: Pgpointer; index_: guint; length: guint): Pgpointer; cdecl; inline; static;
    function set_size(array_: Pgpointer; length: guint): Pgpointer; cdecl; inline; static;
    function sized_new(zero_terminated: gboolean; clear_: gboolean; element_size: guint; reserved_size: guint): Pgpointer; cdecl; inline; static;
    procedure sort(array_: Pgpointer; compare_func: TGCompareFunc); cdecl; inline; static;
    procedure sort_with_data(array_: Pgpointer; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; inline; static;
    procedure unref(array_: Pgpointer); cdecl; inline; static;
  end;
  TGAsciiType = packed object(TBitObject32)
  public
    property alnum: DWord index 1 read GetBit write SetBit;
    property alpha: DWord index 2 read GetBit write SetBit;
    property cntrl: DWord index 4 read GetBit write SetBit;
    property digit: DWord index 8 read GetBit write SetBit;
    property graph: DWord index 16 read GetBit write SetBit;
    property lower: DWord index 32 read GetBit write SetBit;
    property print: DWord index 64 read GetBit write SetBit;
    property punct: DWord index 128 read GetBit write SetBit;
    property space: DWord index 256 read GetBit write SetBit;
    property upper: DWord index 512 read GetBit write SetBit;
    property xdigit: DWord index 1024 read GetBit write SetBit;
  end;


  PPGAsyncQueue = ^PGAsyncQueue;
  PGAsyncQueue = ^TGAsyncQueue;

  PPGTimeVal = ^PGTimeVal;
  PGTimeVal = ^TGTimeVal;

  PPGDestroyNotify = ^PGDestroyNotify;
  PGDestroyNotify = ^TGDestroyNotify;
  TGDestroyNotify = procedure(data: gpointer); cdecl;
  TGAsyncQueue = object
    function length: gint; cdecl; inline;
    function length_unlocked: gint; cdecl; inline;
    procedure lock; cdecl; inline;
    function pop: gpointer; cdecl; inline;
    function pop_unlocked: gpointer; cdecl; inline;
    procedure push(data: gpointer); cdecl; inline;
    procedure push_sorted(data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl; inline;
    procedure push_sorted_unlocked(data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl; inline;
    procedure push_unlocked(data: gpointer); cdecl; inline;
    function ref: PGAsyncQueue; cdecl; inline;
    procedure ref_unlocked; cdecl; inline;
    procedure sort(func: TGCompareDataFunc; user_data: gpointer); cdecl; inline;
    procedure sort_unlocked(func: TGCompareDataFunc; user_data: gpointer); cdecl; inline;
    function timed_pop(end_time: PGTimeVal): gpointer; cdecl; inline;
    function timed_pop_unlocked(end_time: PGTimeVal): gpointer; cdecl; inline;
    function try_pop: gpointer; cdecl; inline;
    function try_pop_unlocked: gpointer; cdecl; inline;
    procedure unlock; cdecl; inline;
    procedure unref; cdecl; inline;
    procedure unref_and_unlock; cdecl; inline;
    function new: PGAsyncQueue; cdecl; inline; static;
    function new_full(item_free_func: TGDestroyNotify): PGAsyncQueue; cdecl; inline; static;
  end;

  PPglong = ^Pglong;
  Pglong = ^glong;
  TGTimeVal = object
    tv_sec: glong;
    tv_usec: glong;
    procedure add(microseconds: glong); cdecl; inline;
    function to_iso8601: Pgchar; cdecl; inline;
    function from_iso8601(iso_date: Pgchar; time_: PGTimeVal): gboolean; cdecl; inline; static;
  end;

  PPGBookmarkFile = ^PGBookmarkFile;
  PGBookmarkFile = ^TGBookmarkFile;

  PPgsize = ^Pgsize;
  Pgsize = ^gsize;
  TGBookmarkFile = object
    procedure add_application(uri: Pgchar; name: Pgchar; exec: Pgchar); cdecl; inline;
    procedure add_group(uri: Pgchar; group: Pgchar); cdecl; inline;
    procedure free; cdecl; inline;
    function get_added(uri: Pgchar): glong; cdecl; inline;
    function get_app_info(uri: Pgchar; name: Pgchar; exec: PPgchar; count: Pguint; stamp: Pglong): gboolean; cdecl; inline;
    function get_applications(uri: Pgchar; length: Pgsize): PPgchar; cdecl; inline;
    function get_description(uri: Pgchar): Pgchar; cdecl; inline;
    function get_groups(uri: Pgchar; length: Pgsize): PPgchar; cdecl; inline;
    function get_icon(uri: Pgchar; href: PPgchar; mime_type: PPgchar): gboolean; cdecl; inline;
    function get_is_private(uri: Pgchar): gboolean; cdecl; inline;
    function get_mime_type(uri: Pgchar): Pgchar; cdecl; inline;
    function get_modified(uri: Pgchar): glong; cdecl; inline;
    function get_size: gint; cdecl; inline;
    function get_title(uri: Pgchar): Pgchar; cdecl; inline;
    function get_uris(length: Pgsize): PPgchar; cdecl; inline;
    function get_visited(uri: Pgchar): glong; cdecl; inline;
    function has_application(uri: Pgchar; name: Pgchar): gboolean; cdecl; inline;
    function has_group(uri: Pgchar; group: Pgchar): gboolean; cdecl; inline;
    function has_item(uri: Pgchar): gboolean; cdecl; inline;
    function load_from_data(data: Pgchar; length: gsize): gboolean; cdecl; inline;
    function load_from_data_dirs(file_: Pgchar; full_path: PPgchar): gboolean; cdecl; inline;
    function load_from_file(filename: Pgchar): gboolean; cdecl; inline;
    function move_item(old_uri: Pgchar; new_uri: Pgchar): gboolean; cdecl; inline;
    function remove_application(uri: Pgchar; name: Pgchar): gboolean; cdecl; inline;
    function remove_group(uri: Pgchar; group: Pgchar): gboolean; cdecl; inline;
    function remove_item(uri: Pgchar): gboolean; cdecl; inline;
    procedure set_added(uri: Pgchar; added: glong); cdecl; inline;
    function set_app_info(uri: Pgchar; name: Pgchar; exec: Pgchar; count: gint; stamp: glong): gboolean; cdecl; inline;
    procedure set_description(uri: Pgchar; description: Pgchar); cdecl; inline;
    procedure set_groups(uri: Pgchar; groups: PPgchar; length: gsize); cdecl; inline;
    procedure set_icon(uri: Pgchar; href: Pgchar; mime_type: Pgchar); cdecl; inline;
    procedure set_is_private(uri: Pgchar; is_private: gboolean); cdecl; inline;
    procedure set_mime_type(uri: Pgchar; mime_type: Pgchar); cdecl; inline;
    procedure set_modified(uri: Pgchar; modified: glong); cdecl; inline;
    procedure set_title(uri: Pgchar; title: Pgchar); cdecl; inline;
    procedure set_visited(uri: Pgchar; visited: glong); cdecl; inline;
    function to_data(length: Pgsize): Pgchar; cdecl; inline;
    function to_file(filename: Pgchar): gboolean; cdecl; inline;
    function error_quark: TGQuark; cdecl; inline; static;
    function new: PGBookmarkFile; cdecl; inline; static;
  end;

  PPGBookmarkFileError = ^PGBookmarkFileError;
  PGBookmarkFileError = ^TGBookmarkFileError;

  PPGByteArray = ^PGByteArray;
  PGByteArray = ^TGByteArray;

  PPPPPPPPguint8 = ^PPPPPPPguint8;
  PPPPPPPguint8 = ^PPPPPPguint8;
  PPPPPPguint8 = ^PPPPPguint8;
  PPPPPguint8 = ^PPPPguint8;
  PPPPguint8 = ^PPPguint8;
  PPPguint8 = ^PPguint8;
  PPguint8 = ^Pguint8;
  Pguint8 = ^guint8;
  TGByteArray = object
    data: Pguint8;
    len: guint;
    function append(array_: Pguint8; data: Pguint8; len: guint): Pguint8; cdecl; inline; static;
    function free(array_: Pguint8; free_segment: gboolean): Pguint8; cdecl; inline; static;
    function new: Pguint8; cdecl; inline; static;
    function prepend(array_: Pguint8; data: Pguint8; len: guint): Pguint8; cdecl; inline; static;
    function ref(array_: Pguint8): Pguint8; cdecl; inline; static;
    function remove_index(array_: Pguint8; index_: guint): Pguint8; cdecl; inline; static;
    function remove_index_fast(array_: Pguint8; index_: guint): Pguint8; cdecl; inline; static;
    function remove_range(array_: Pguint8; index_: guint; length: guint): Pguint8; cdecl; inline; static;
    function set_size(array_: Pguint8; length: guint): Pguint8; cdecl; inline; static;
    function sized_new(reserved_size: guint): Pguint8; cdecl; inline; static;
    procedure sort(array_: Pguint8; compare_func: TGCompareFunc); cdecl; inline; static;
    procedure sort_with_data(array_: Pguint8; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; inline; static;
    procedure unref(array_: Pguint8); cdecl; inline; static;
  end;
  TGHFunc = procedure(key: gpointer; value: gpointer; user_data: gpointer); cdecl;

  PPGCache = ^PGCache;
  PGCache = ^TGCache;

  PPGHFunc = ^PGHFunc;
  PGHFunc = ^TGHFunc;

  PPGCacheNewFunc = ^PGCacheNewFunc;
  PGCacheNewFunc = ^TGCacheNewFunc;
  TGCacheNewFunc = function(key: gpointer): gpointer; cdecl;

  PPGCacheDestroyFunc = ^PGCacheDestroyFunc;
  PGCacheDestroyFunc = ^TGCacheDestroyFunc;
  TGCacheDestroyFunc = procedure(value: gpointer); cdecl;

  PPGCacheDupFunc = ^PGCacheDupFunc;
  PGCacheDupFunc = ^TGCacheDupFunc;
  TGCacheDupFunc = function(value: gpointer): gpointer; cdecl;

  PPGHashFunc = ^PGHashFunc;
  PGHashFunc = ^TGHashFunc;
  TGHashFunc = function(key: gpointer): guint; cdecl;

  PPGEqualFunc = ^PGEqualFunc;
  PGEqualFunc = ^TGEqualFunc;
  TGEqualFunc = function(a: gpointer; b: gpointer): gboolean; cdecl;
  TGCache = object
    procedure destroy_; cdecl; inline;
    function insert(key: gpointer): gpointer; cdecl; inline;
    procedure key_foreach(func: TGHFunc; user_data: gpointer); cdecl; inline;
    procedure remove(value: gpointer); cdecl; inline;
    procedure value_foreach(func: TGHFunc; user_data: gpointer); cdecl; inline;
    function new(value_new_func: TGCacheNewFunc; value_destroy_func: TGCacheDestroyFunc; key_dup_func: TGCacheDupFunc; key_destroy_func: TGCacheDestroyFunc; hash_key_func: TGHashFunc; hash_value_func: TGHashFunc; key_equal_func: TGEqualFunc): PGCache; cdecl; inline; static;
  end;

  PPGChecksum = ^PGChecksum;
  PGChecksum = ^TGChecksum;

  PPgssize = ^Pgssize;
  Pgssize = ^gssize;

  PPGChecksumType = ^PGChecksumType;
  PGChecksumType = ^TGChecksumType;
  TGChecksum = object
    function copy: PGChecksum; cdecl; inline;
    procedure free; cdecl; inline;
    procedure get_digest(buffer: Pguint8; digest_len: Pgsize); cdecl; inline;
    function get_string: Pgchar; cdecl; inline;
    procedure reset; cdecl; inline;
    procedure update(data: Pguint8; length: gssize); cdecl; inline;
    function new(checksum_type: TGChecksumType): PGChecksum; cdecl; inline; static;
    function type_get_length(checksum_type: TGChecksumType): gssize; cdecl; inline; static;
  end;
  TGChildWatchFunc = procedure(pid: TGPid; status: gint; data: gpointer); cdecl;

  PPGList = ^PGList;
  PGList = ^TGList;

  PPGFunc = ^PGFunc;
  PGFunc = ^TGFunc;
  TGFunc = procedure(data: gpointer; user_data: gpointer); cdecl;
  TGList = object
    data: gpointer;
    next: PGList;
    prev: PGList;
    function alloc: PGList; cdecl; inline; static;
    function append(list: PGList; data: gpointer): PGList; cdecl; inline; static;
    function concat(list1: PGList; list2: PGList): PGList; cdecl; inline; static;
    function copy(list: PGList): PGList; cdecl; inline; static;
    function delete_link(list: PGList; link_: PGList): PGList; cdecl; inline; static;
    function find(list: PGList; data: gpointer): PGList; cdecl; inline; static;
    function find_custom(list: PGList; data: gpointer; func: TGCompareFunc): PGList; cdecl; inline; static;
    function first(list: PGList): PGList; cdecl; inline; static;
    procedure foreach(list: PGList; func: TGFunc; user_data: gpointer); cdecl; inline; static;
    procedure free(list: PGList); cdecl; inline; static;
    procedure free_1(list: PGList); cdecl; inline; static;
    procedure free_full(list: PGList; free_func: TGDestroyNotify); cdecl; inline; static;
    function index(list: PGList; data: gpointer): gint; cdecl; inline; static;
    function insert(list: PGList; data: gpointer; position: gint): PGList; cdecl; inline; static;
    function insert_before(list: PGList; sibling: PGList; data: gpointer): PGList; cdecl; inline; static;
    function insert_sorted(list: PGList; data: gpointer; func: TGCompareFunc): PGList; cdecl; inline; static;
    function insert_sorted_with_data(list: PGList; data: gpointer; func: TGCompareDataFunc; user_data: gpointer): PGList; cdecl; inline; static;
    function last(list: PGList): PGList; cdecl; inline; static;
    function length(list: PGList): guint; cdecl; inline; static;
    function nth(list: PGList; n: guint): PGList; cdecl; inline; static;
    function nth_data(list: PGList; n: guint): gpointer; cdecl; inline; static;
    function nth_prev(list: PGList; n: guint): PGList; cdecl; inline; static;
    procedure pop_allocator; cdecl; inline; static;
    function position(list: PGList; llink: PGList): gint; cdecl; inline; static;
    function prepend(list: PGList; data: gpointer): PGList; cdecl; inline; static;
    procedure push_allocator(allocator: gpointer); cdecl; inline; static;
    function remove(list: PGList; data: gpointer): PGList; cdecl; inline; static;
    function remove_all(list: PGList; data: gpointer): PGList; cdecl; inline; static;
    function remove_link(list: PGList; llink: PGList): PGList; cdecl; inline; static;
    function reverse(list: PGList): PGList; cdecl; inline; static;
    function sort(list: PGList; compare_func: TGCompareFunc): PGList; cdecl; inline; static;
    function sort_with_data(list: PGList; compare_func: TGCompareDataFunc; user_data: gpointer): PGList; cdecl; inline; static;
  end;
  TGCompletionFunc = function(param0: gpointer): Pgchar; cdecl;
  TGCompletionStrncmpFunc = function(s1: Pgchar; s2: Pgchar; n: gsize): gint; cdecl;

  PPGCompletion = ^PGCompletion;
  PGCompletion = ^TGCompletion;

  PPGCompletionFunc = ^PGCompletionFunc;
  PGCompletionFunc = ^TGCompletionFunc;

  PPGCompletionStrncmpFunc = ^PGCompletionStrncmpFunc;
  PGCompletionStrncmpFunc = ^TGCompletionStrncmpFunc;
  TGCompletion = object
    items: PGList;
    func: TGCompletionFunc;
    prefix: Pgchar;
    cache: PGList;
    strncmp_func: TGCompletionStrncmpFunc;
    
    
    
    
    
    
    
    function new(func: TGCompletionFunc): PGCompletion; cdecl; inline; static;
  end;

  PPGCond = ^PGCond;
  PGCond = ^TGCond;

  TGCond = record
    Unknown: Pointer;
  end;



  PPGConvertError = ^PGConvertError;
  PGConvertError = ^TGConvertError;
  TGCopyFunc = function(src: gpointer; data: gpointer): gpointer; cdecl;

  PPGData = ^PGData;
  PGData = ^TGData;

  TGData = record
    Unknown: Pointer;
  end;


  TGDataForeachFunc = procedure(key_id: TGQuark; data: gpointer; user_data: gpointer); cdecl;

  PPGDate = ^PGDate;
  PGDate = ^TGDate;

  PPGDateMonth = ^PGDateMonth;
  PGDateMonth = ^TGDateMonth;

  PPguint32 = ^Pguint32;
  Pguint32 = ^guint32;

  PPGDateWeekday = ^PGDateWeekday;
  PGDateWeekday = ^TGDateWeekday;
  TGDate = object
    julian_days: guint32 { changed from guint to accomodate 32 bitsize requirement };
    julian: guint1 { changed from guint to accomodate 1 bitsize requirement };
    dmy: guint1 { changed from guint to accomodate 1 bitsize requirement };
    day: guint6 { changed from guint to accomodate 6 bitsize requirement };
    month: guint4 { changed from guint to accomodate 4 bitsize requirement };
    year: guint16 { changed from guint to accomodate 16 bitsize requirement };
    function new: PGDate; cdecl; inline; static;
    function new_dmy(day: TGDateDay; month: TGDateMonth; year: TGDateYear): PGDate; cdecl; inline; static;
    function new_julian(julian_day: guint32): PGDate; cdecl; inline; static;
    procedure add_days(n_days: guint); cdecl; inline;
    procedure add_months(n_months: guint); cdecl; inline;
    procedure add_years(n_years: guint); cdecl; inline;
    procedure clamp(min_date: PGDate; max_date: PGDate); cdecl; inline;
    procedure clear(n_dates: guint); cdecl; inline;
    function compare(rhs: PGDate): gint; cdecl; inline;
    function days_between(date2: PGDate): gint; cdecl; inline;
    procedure free; cdecl; inline;
    function get_day: TGDateDay; cdecl; inline;
    function get_day_of_year: guint; cdecl; inline;
    function get_iso8601_week_of_year: guint; cdecl; inline;
    function get_julian: guint32; cdecl; inline;
    function get_monday_week_of_year: guint; cdecl; inline;
    function get_month: TGDateMonth; cdecl; inline;
    function get_sunday_week_of_year: guint; cdecl; inline;
    function get_weekday: TGDateWeekday; cdecl; inline;
    function get_year: TGDateYear; cdecl; inline;
    function is_first_of_month: gboolean; cdecl; inline;
    function is_last_of_month: gboolean; cdecl; inline;
    procedure order(date2: PGDate); cdecl; inline;
    procedure set_day(day: TGDateDay); cdecl; inline;
    procedure set_dmy(day: TGDateDay; month: TGDateMonth; y: TGDateYear); cdecl; inline;
    procedure set_julian(julian_date: guint32); cdecl; inline;
    procedure set_month(month: TGDateMonth); cdecl; inline;
    procedure set_parse(str: Pgchar); cdecl; inline;
    
    procedure set_time_t(timet: glong); cdecl; inline;
    procedure set_time_val(timeval: PGTimeVal); cdecl; inline;
    procedure set_year(year: TGDateYear); cdecl; inline;
    procedure subtract_days(n_days: guint); cdecl; inline;
    procedure subtract_months(n_months: guint); cdecl; inline;
    procedure subtract_years(n_years: guint); cdecl; inline;
    procedure to_struct_tm(tm: Pgpointer); cdecl; inline;
    function valid: gboolean; cdecl; inline;
    function get_days_in_month(month: TGDateMonth; year: TGDateYear): guint8; cdecl; inline; static;
    function get_monday_weeks_in_year(year: TGDateYear): guint8; cdecl; inline; static;
    function get_sunday_weeks_in_year(year: TGDateYear): guint8; cdecl; inline; static;
    function is_leap_year(year: TGDateYear): gboolean; cdecl; inline; static;
    function strftime(s: Pgchar; slen: gsize; format: Pgchar; date: PGDate): gsize; cdecl; inline; static;
    function valid_day(day: TGDateDay): gboolean; cdecl; inline; static;
    function valid_dmy(day: TGDateDay; month: TGDateMonth; year: TGDateYear): gboolean; cdecl; inline; static;
    function valid_julian(julian_date: guint32): gboolean; cdecl; inline; static;
    function valid_month(month: TGDateMonth): gboolean; cdecl; inline; static;
    function valid_weekday(weekday: TGDateWeekday): gboolean; cdecl; inline; static;
    function valid_year(year: TGDateYear): gboolean; cdecl; inline; static;
  end;

  PPGDateDMY = ^PGDateDMY;
  PGDateDMY = ^TGDateDMY;

  PPGDateTime = ^PGDateTime;
  PGDateTime = ^TGDateTime;

  PPGTimeZone = ^PGTimeZone;
  PGTimeZone = ^TGTimeZone;

  PPgdouble = ^Pgdouble;
  Pgdouble = ^gdouble;

  PPgint64 = ^Pgint64;
  Pgint64 = ^gint64;

  PPGSource = ^PGSource;
  PGSource = ^TGSource;
  TGDateTime = object
    function new(tz: PGTimeZone; year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl; inline; static;
    function new_from_timeval_local(tv: PGTimeVal): PGDateTime; cdecl; inline; static;
    function new_from_timeval_utc(tv: PGTimeVal): PGDateTime; cdecl; inline; static;
    function new_from_unix_local(t: gint64): PGDateTime; cdecl; inline; static;
    function new_from_unix_utc(t: gint64): PGDateTime; cdecl; inline; static;
    function new_local(year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl; inline; static;
    function new_now(tz: PGTimeZone): PGDateTime; cdecl; inline; static;
    function new_now_local: PGDateTime; cdecl; inline; static;
    function new_now_utc: PGDateTime; cdecl; inline; static;
    function new_utc(year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl; inline; static;
    function add(timespan: TGTimeSpan): PGDateTime; cdecl; inline;
    function add_days(days: gint): PGDateTime; cdecl; inline;
    function add_full(years: gint; months: gint; days: gint; hours: gint; minutes: gint; seconds: gdouble): PGDateTime; cdecl; inline;
    function add_hours(hours: gint): PGDateTime; cdecl; inline;
    function add_minutes(minutes: gint): PGDateTime; cdecl; inline;
    function add_months(months: gint): PGDateTime; cdecl; inline;
    function add_seconds(seconds: gdouble): PGDateTime; cdecl; inline;
    function add_weeks(weeks: gint): PGDateTime; cdecl; inline;
    function add_years(years: gint): PGDateTime; cdecl; inline;
    function difference(begin_: PGDateTime): TGTimeSpan; cdecl; inline;
    function format(format: Pgchar): Pgchar; cdecl; inline;
    function get_day_of_month: gint; cdecl; inline;
    function get_day_of_week: gint; cdecl; inline;
    function get_day_of_year: gint; cdecl; inline;
    function get_hour: gint; cdecl; inline;
    function get_microsecond: gint; cdecl; inline;
    function get_minute: gint; cdecl; inline;
    function get_month: gint; cdecl; inline;
    function get_second: gint; cdecl; inline;
    function get_seconds: gdouble; cdecl; inline;
    function get_timezone_abbreviation: Pgchar; cdecl; inline;
    function get_utc_offset: TGTimeSpan; cdecl; inline;
    function get_week_numbering_year: gint; cdecl; inline;
    function get_week_of_year: gint; cdecl; inline;
    function get_year: gint; cdecl; inline;
    procedure get_ymd(year: Pgint; month: Pgint; day: Pgint); cdecl; inline;
    function is_daylight_savings: gboolean; cdecl; inline;
    function ref: PGDateTime; cdecl; inline;
    function source_new(cancel_on_set: gboolean): PGSource; cdecl; inline;
    function to_local: PGDateTime; cdecl; inline;
    function to_timeval(tv: PGTimeVal): gboolean; cdecl; inline;
    function to_timezone(tz: PGTimeZone): PGDateTime; cdecl; inline;
    function to_unix: gint64; cdecl; inline;
    function to_utc: PGDateTime; cdecl; inline;
    procedure unref; cdecl; inline;
    function compare(dt1: gpointer; dt2: gpointer): gint; cdecl; inline; static;
    function equal(dt1: gpointer; dt2: gpointer): gboolean; cdecl; inline; static;
    function hash(datetime: gpointer): guint; cdecl; inline; static;
  end;

  PPGTimeType = ^PGTimeType;
  PGTimeType = ^TGTimeType;

  PPgint32 = ^Pgint32;
  Pgint32 = ^gint32;
  TGTimeZone = object
    function adjust_time(type_: TGTimeType; time_: Pgint64): gint; cdecl; inline;
    function find_interval(type_: TGTimeType; time_: gint64): gint; cdecl; inline;
    function get_abbreviation(interval: gint): Pgchar; cdecl; inline;
    function get_offset(interval: gint): gint32; cdecl; inline;
    function is_dst(interval: gint): gboolean; cdecl; inline;
    function ref: PGTimeZone; cdecl; inline;
    procedure unref; cdecl; inline;
    function new(identifier: Pgchar): PGTimeZone; cdecl; inline; static;
    function new_local: PGTimeZone; cdecl; inline; static;
    function new_utc: PGTimeZone; cdecl; inline; static;
  end;

  PPPGPollFD = ^PPGPollFD;
  PPGPollFD = ^PGPollFD;
  PGPollFD = ^TGPollFD;

  PPGMainContext = ^PGMainContext;
  PGMainContext = ^TGMainContext;

  PPGSourceFunc = ^PGSourceFunc;
  PGSourceFunc = ^TGSourceFunc;
  TGSourceFunc = function(data: gpointer): gboolean; cdecl;

  PPGSourceCallbackFuncs = ^PGSourceCallbackFuncs;
  PGSourceCallbackFuncs = ^TGSourceCallbackFuncs;

  PPGSourceFuncs = ^PGSourceFuncs;
  PGSourceFuncs = ^TGSourceFuncs;

  PPGSList = ^PGSList;
  PGSList = ^TGSList;

  PPGSourcePrivate = ^PGSourcePrivate;
  PGSourcePrivate = ^TGSourcePrivate;
  TGSource = object
    callback_data: gpointer;
    callback_funcs: PGSourceCallbackFuncs;
    source_funcs: PGSourceFuncs;
    ref_count: guint;
    context: PGMainContext;
    priority: gint;
    flags: guint;
    source_id: guint;
    poll_fds: PGSList;
    prev: PGSource;
    next: PGSource;
    name: Pgchar;
    priv: PGSourcePrivate;
    procedure add_child_source(child_source: PGSource); cdecl; inline;
    procedure add_poll(fd: PGPollFD); cdecl; inline;
    function attach(context: PGMainContext): guint; cdecl; inline;
    procedure destroy_; cdecl; inline;
    function get_can_recurse: gboolean; cdecl; inline;
    function get_context: PGMainContext; cdecl; inline;
    
    function get_id: guint; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_priority: gint; cdecl; inline;
    function get_time: gint64; cdecl; inline;
    function is_destroyed: gboolean; cdecl; inline;
    function ref: PGSource; cdecl; inline;
    procedure remove_child_source(child_source: PGSource); cdecl; inline;
    procedure remove_poll(fd: PGPollFD); cdecl; inline;
    procedure set_callback(func: TGSourceFunc; data: gpointer; notify: TGDestroyNotify); cdecl; inline;
    procedure set_callback_indirect(callback_data: gpointer; callback_funcs: PGSourceCallbackFuncs); cdecl; inline;
    procedure set_can_recurse(can_recurse: gboolean); cdecl; inline;
    procedure set_funcs(funcs: PGSourceFuncs); cdecl; inline;
    procedure set_name(name: Pgchar); cdecl; inline;
    procedure set_priority(priority: gint); cdecl; inline;
    procedure unref; cdecl; inline;
    function new(source_funcs: PGSourceFuncs; struct_size: guint): PGSource; cdecl; inline; static;
    function remove(tag: guint): gboolean; cdecl; inline; static;
    function remove_by_funcs_user_data(funcs: PGSourceFuncs; user_data: gpointer): gboolean; cdecl; inline; static;
    function remove_by_user_data(user_data: gpointer): gboolean; cdecl; inline; static;
    procedure set_name_by_id(tag: guint; name: Pgchar); cdecl; inline; static;
  end;

  PPGDebugKey = ^PGDebugKey;
  PGDebugKey = ^TGDebugKey;

  TGDebugKey = record
    key: Pgchar;
    value: guint;
  end;



  PPGDir = ^PGDir;
  PGDir = ^TGDir;
  TGDir = object
    procedure close; cdecl; inline;
    function read_name: Pgchar; cdecl; inline;
    procedure rewind; cdecl; inline;
    function make_tmp(tmpl: Pgchar): Pgchar; cdecl; inline; static;
    function open(path: Pgchar; flags: guint): PGDir; cdecl; inline; static;
  end;
  TGDoubleIEEE754 = record
    case longint of
      0 : (v_double: gdouble);
      1 : (
        mpn : record
          mantissa_low: guint32 { changed from guint to accomodate 32 bitsize requirement };
          mantissa_high: guint20 { changed from guint to accomodate 20 bitsize requirement };
          biased_exponent: guint11 { changed from guint to accomodate 11 bitsize requirement };
          sign: guint1 { changed from guint to accomodate 1 bitsize requirement };
        end;

);
  end;



  PPGError = ^PGError;
  PGError = ^TGError;

  Pva_list = ^Tva_list;

  { va_list }
  Tva_list = record
    { opaque type }
    Unknown: Pointer;
  end;

  TGError = object
    domain: TGQuark;
    code: gint;
    message: Pgchar;
    //function new(domain: TGQuark; code: gint; format: Pgchar; args: array of const): PGError; cdecl; inline; static;
    function new_literal(domain: TGQuark; code: gint; message: Pgchar): PGError; cdecl; inline; static;
    //function new_valist(domain: TGQuark; code: gint; format: Pgchar; args: Tva_list): PGError; cdecl; inline; static;
    function copy: PGError; cdecl; inline;
    procedure free; cdecl; inline;
    function matches(domain: TGQuark; code: gint): gboolean; cdecl; inline;
  end;

  PPGErrorType = ^PGErrorType;
  PGErrorType = ^TGErrorType;

  PPGFileError = ^PGFileError;
  PGFileError = ^TGFileError;
  TGFileTest = packed object(TBitObject32)
  public
    property is_regular: DWord index 1 read GetBit write SetBit;
    property is_symlink: DWord index 2 read GetBit write SetBit;
    property is_dir: DWord index 4 read GetBit write SetBit;
    property is_executable: DWord index 8 read GetBit write SetBit;
    property exists: DWord index 16 read GetBit write SetBit;
  end;


  PPgfloat = ^Pgfloat;
  Pgfloat = ^gfloat;
  TGFloatIEEE754 = record
    case longint of
      0 : (v_float: gfloat);
      1 : (
        mpn : record
          mantissa: guint23 { changed from guint to accomodate 23 bitsize requirement };
          biased_exponent: guint8 { changed from guint to accomodate 8 bitsize requirement };
          sign: guint1 { changed from guint to accomodate 1 bitsize requirement };
        end;

);
  end;


  TGFormatSizeFlags = packed object(TBitObject32)
  public
    property default_: DWord index 0 read GetBit write SetBit;
    property long_format: DWord index 1 read GetBit write SetBit;
    property iec_units: DWord index 2 read GetBit write SetBit;
  end;

  TGFreeFunc = procedure(data: gpointer); cdecl;
  TGHRFunc = function(key: gpointer; value: gpointer; user_data: gpointer): gboolean; cdecl;

  PPGHashTable = ^PGHashTable;
  PGHashTable = ^TGHashTable;

  PPGHRFunc = ^PGHRFunc;
  PGHRFunc = ^TGHRFunc;
  TGHashTable = object
    procedure destroy_(hash_table: PGHashTable); cdecl; inline; static;
    function find(hash_table: PGHashTable; predicate: TGHRFunc; user_data: gpointer): gpointer; cdecl; inline; static;
    procedure foreach(hash_table: PGHashTable; func: TGHFunc; user_data: gpointer); cdecl; inline; static;
    function foreach_remove(hash_table: PGHashTable; func: TGHRFunc; user_data: gpointer): guint; cdecl; inline; static;
    function foreach_steal(hash_table: PGHashTable; func: TGHRFunc; user_data: gpointer): guint; cdecl; inline; static;
    function get_keys(hash_table: PGHashTable): PGList; cdecl; inline; static;
    function get_values(hash_table: PGHashTable): PGList; cdecl; inline; static;
    procedure insert(hash_table: PGHashTable; key: gpointer; value: gpointer); cdecl; inline; static;
    function lookup(hash_table: PGHashTable; key: gpointer): gpointer; cdecl; inline; static;
    function lookup_extended(hash_table: PGHashTable; lookup_key: gpointer; orig_key: Pgpointer; value: Pgpointer): gboolean; cdecl; inline; static;
    function new(hash_func: TGHashFunc; key_equal_func: TGEqualFunc): PGHashTable; cdecl; inline; static;
    function new_full(hash_func: TGHashFunc; key_equal_func: TGEqualFunc; key_destroy_func: TGDestroyNotify; value_destroy_func: TGDestroyNotify): PGHashTable; cdecl; inline; static;
    function ref(hash_table: PGHashTable): PGHashTable; cdecl; inline; static;
    function remove(hash_table: PGHashTable; key: gpointer): gboolean; cdecl; inline; static;
    procedure remove_all(hash_table: PGHashTable); cdecl; inline; static;
    procedure replace(hash_table: PGHashTable; key: gpointer; value: gpointer); cdecl; inline; static;
    function size(hash_table: PGHashTable): guint; cdecl; inline; static;
    function steal(hash_table: PGHashTable; key: gpointer): gboolean; cdecl; inline; static;
    procedure steal_all(hash_table: PGHashTable); cdecl; inline; static;
    procedure unref(hash_table: PGHashTable); cdecl; inline; static;
  end;

  PPGHashTableIter = ^PGHashTableIter;
  PGHashTableIter = ^TGHashTableIter;
  TGHashTableIter = object
    dummy1: gpointer;
    dummy2: gpointer;
    dummy3: gpointer;
    dummy4: gint;
    dummy5: gboolean;
    dummy6: gpointer;
    function get_hash_table: PGHashTable; cdecl; inline;
    procedure init(hash_table: PGHashTable); cdecl; inline;
    function next(key: Pgpointer; value: Pgpointer): gboolean; cdecl; inline;
    procedure remove; cdecl; inline;
    procedure replace(value: gpointer); cdecl; inline;
    procedure steal; cdecl; inline;
  end;

  PPGHmac = ^PGHmac;
  PGHmac = ^TGHmac;
  TGHmac = object
    function copy: PGHmac; cdecl; inline;
    procedure get_digest(buffer: Pguint8; digest_len: Pgsize); cdecl; inline;
    function get_string: Pgchar; cdecl; inline;
    function ref: PGHmac; cdecl; inline;
    procedure unref; cdecl; inline;
    procedure update(data: Pguint8; length: gssize); cdecl; inline;
    function new(digest_type: TGChecksumType; key: Pguint8; key_len: gsize): PGHmac; cdecl; inline; static;
  end;

  PPGHook = ^PGHook;
  PGHook = ^TGHook;

  PPGHookList = ^PGHookList;
  PGHookList = ^TGHookList;

  PPgulong = ^Pgulong;
  Pgulong = ^gulong;

  PPGHookFindFunc = ^PGHookFindFunc;
  PGHookFindFunc = ^TGHookFindFunc;
  TGHookFindFunc = function(hook: PGHook; data: gpointer): gboolean; cdecl;

  PPGHookCompareFunc = ^PGHookCompareFunc;
  PGHookCompareFunc = ^TGHookCompareFunc;
  TGHookCompareFunc = function(new_hook: PGHook; sibling: PGHook): gint; cdecl;
  TGHook = object
    data: gpointer;
    next: PGHook;
    prev: PGHook;
    ref_count: guint;
    hook_id: gulong;
    flags: guint;
    func: gpointer;
    destroy_1: TGDestroyNotify;
    function compare_ids(sibling: PGHook): gint; cdecl; inline;
    function alloc(hook_list: PGHookList): PGHook; cdecl; inline; static;
    function destroy_(hook_list: PGHookList; hook_id: gulong): gboolean; cdecl; inline; static;
    procedure destroy_link(hook_list: PGHookList; hook: PGHook); cdecl; inline; static;
    function find(hook_list: PGHookList; need_valids: gboolean; func: TGHookFindFunc; data: gpointer): PGHook; cdecl; inline; static;
    function find_data(hook_list: PGHookList; need_valids: gboolean; data: gpointer): PGHook; cdecl; inline; static;
    function find_func(hook_list: PGHookList; need_valids: gboolean; func: gpointer): PGHook; cdecl; inline; static;
    function find_func_data(hook_list: PGHookList; need_valids: gboolean; func: gpointer; data: gpointer): PGHook; cdecl; inline; static;
    function first_valid(hook_list: PGHookList; may_be_in_call: gboolean): PGHook; cdecl; inline; static;
    procedure free(hook_list: PGHookList; hook: PGHook); cdecl; inline; static;
    function get(hook_list: PGHookList; hook_id: gulong): PGHook; cdecl; inline; static;
    procedure insert_before(hook_list: PGHookList; sibling: PGHook; hook: PGHook); cdecl; inline; static;
    procedure insert_sorted(hook_list: PGHookList; hook: PGHook; func: TGHookCompareFunc); cdecl; inline; static;
    function next_valid(hook_list: PGHookList; hook: PGHook; may_be_in_call: gboolean): PGHook; cdecl; inline; static;
    procedure prepend(hook_list: PGHookList; hook: PGHook); cdecl; inline; static;
    function ref(hook_list: PGHookList; hook: PGHook): PGHook; cdecl; inline; static;
    procedure unref(hook_list: PGHookList; hook: PGHook); cdecl; inline; static;
  end;

  PPGHookMarshaller = ^PGHookMarshaller;
  PGHookMarshaller = ^TGHookMarshaller;
  TGHookMarshaller = procedure(hook: PGHook; marshal_data: gpointer); cdecl;

  PPGHookCheckMarshaller = ^PGHookCheckMarshaller;
  PGHookCheckMarshaller = ^TGHookCheckMarshaller;
  TGHookCheckMarshaller = function(hook: PGHook; marshal_data: gpointer): gboolean; cdecl;

  PPGHookFinalizeFunc = ^PGHookFinalizeFunc;
  PGHookFinalizeFunc = ^TGHookFinalizeFunc;
  TGHookFinalizeFunc = procedure(hook_list: PGHookList; hook: PGHook); cdecl;
  TGHookList = object
    seq_id: gulong;
    hook_size: guint16 { changed from guint to accomodate 16 bitsize requirement };
    is_setup: guint1 { changed from guint to accomodate 1 bitsize requirement };
    hooks: PGHook;
    dummy3: gpointer;
    finalize_hook: TGHookFinalizeFunc;
    dummy: array [0..1] of gpointer;
    procedure clear; cdecl; inline;
    procedure init(hook_size: guint); cdecl; inline;
    procedure invoke(may_recurse: gboolean); cdecl; inline;
    procedure invoke_check(may_recurse: gboolean); cdecl; inline;
    procedure marshal(may_recurse: gboolean; marshaller: TGHookMarshaller; marshal_data: gpointer); cdecl; inline;
    procedure marshal_check(may_recurse: gboolean; marshaller: TGHookCheckMarshaller; marshal_data: gpointer); cdecl; inline;
  end;
  TGHookCheckFunc = function(data: gpointer): gboolean; cdecl;
  TGHookFlagMask = packed object(TBitObject32)
  public
    property active: DWord index 1 read GetBit write SetBit;
    property in_call: DWord index 2 read GetBit write SetBit;
    property mask: DWord index 15 read GetBit write SetBit;
  end;

  TGHookFunc = procedure(data: gpointer); cdecl;

  PPGIConv = ^PGIConv;
  PGIConv = ^TGIConv;
  TGIConv = object
    function g_iconv(inbuf: PPgchar; inbytes_left: Pgsize; outbuf: PPgchar; outbytes_left: Pgsize): gsize; cdecl; inline;
    function close: gint; cdecl; inline;
    function open(to_codeset: Pgchar; from_codeset: Pgchar): TGIConv; cdecl; inline; static;
  end;

  PPGIOFuncs = ^PGIOFuncs;
  PGIOFuncs = ^TGIOFuncs;

  PPGIOStatus = ^PGIOStatus;
  PGIOStatus = ^TGIOStatus;

  PPGIOChannel = ^PGIOChannel;
  PGIOChannel = ^TGIOChannel;

  PPGSeekType = ^PGSeekType;
  PGSeekType = ^TGSeekType;

  PPGIOCondition = ^PGIOCondition;
  PGIOCondition = ^TGIOCondition;
  TGIOCondition = packed object(TBitObject32)
  public
    property in_: DWord index 1 read GetBit write SetBit;
    property out_: DWord index 4 read GetBit write SetBit;
    property pri: DWord index 2 read GetBit write SetBit;
    property err: DWord index 8 read GetBit write SetBit;
    property hup: DWord index 16 read GetBit write SetBit;
    property nval: DWord index 32 read GetBit write SetBit;
  end;


  PPGIOFlags = ^PGIOFlags;
  PGIOFlags = ^TGIOFlags;
  TGIOFlags = packed object(TBitObject32)
  public
    property append: DWord index 1 read GetBit write SetBit;
    property nonblock: DWord index 2 read GetBit write SetBit;
    property is_readable: DWord index 4 read GetBit write SetBit;
    property is_writeable: DWord index 8 read GetBit write SetBit;
    property is_seekable: DWord index 16 read GetBit write SetBit;
    property mask: DWord index 31 read GetBit write SetBit;
    property get_mask: DWord index 31 read GetBit write SetBit;
    property set_mask: DWord index 3 read GetBit write SetBit;
  end;


  TGIOFuncs = record
    io_read: function(channel: PGIOChannel; buf: Pgchar; count: gsize; bytes_read: Pgsize): TGIOStatus; cdecl;
    io_write: function(channel: PGIOChannel; buf: Pgchar; count: gsize; bytes_written: Pgsize): TGIOStatus; cdecl;
    io_seek: function(channel: PGIOChannel; offset: gint64; type_: TGSeekType): TGIOStatus; cdecl;
    io_close: function(channel: PGIOChannel): TGIOStatus; cdecl;
    io_create_watch: function(channel: PGIOChannel; condition: TGIOCondition): PGSource; cdecl;
    io_free: procedure(channel: PGIOChannel); cdecl;
    io_set_flags: function(channel: PGIOChannel; flags: TGIOFlags): TGIOStatus; cdecl;
    io_get_flags: function(channel: PGIOChannel): TGIOFlags; cdecl;
  end;



  PPGString = ^PGString;
  PGString = ^TGString;

  PPgunichar = ^Pgunichar;
  Pgunichar = ^gunichar;
  TGString = object
    str: Pgchar;
    len: gsize;
    allocated_len: gsize;
    function append(val: Pgchar): PGString; cdecl; inline;
    function append_c(c: gchar): PGString; cdecl; inline;
    function append_len(val: Pgchar; len: gssize): PGString; cdecl; inline;
    //procedure append_printf(format: Pgchar; args: array of const); cdecl; inline;
    function append_unichar(wc: gunichar): PGString; cdecl; inline;
    function append_uri_escaped(unescaped: Pgchar; reserved_chars_allowed: Pgchar; allow_utf8: gboolean): PGString; cdecl; inline;
    //procedure append_vprintf(format: Pgchar; args: Tva_list); cdecl; inline;
    function ascii_down: PGString; cdecl; inline;
    function ascii_up: PGString; cdecl; inline;
    function assign(rval: Pgchar): PGString; cdecl; inline;
    function down: PGString; cdecl; inline;
    function equal(v2: PGString): gboolean; cdecl; inline;
    function erase(pos: gssize; len: gssize): PGString; cdecl; inline;
    function free(free_segment: gboolean): Pgchar; cdecl; inline;
    function hash: guint; cdecl; inline;
    function insert(pos: gssize; val: Pgchar): PGString; cdecl; inline;
    function insert_c(pos: gssize; c: gchar): PGString; cdecl; inline;
    function insert_len(pos: gssize; val: Pgchar; len: gssize): PGString; cdecl; inline;
    function insert_unichar(pos: gssize; wc: gunichar): PGString; cdecl; inline;
    function overwrite(pos: gsize; val: Pgchar): PGString; cdecl; inline;
    function overwrite_len(pos: gsize; val: Pgchar; len: gssize): PGString; cdecl; inline;
    function prepend(val: Pgchar): PGString; cdecl; inline;
    function prepend_c(c: gchar): PGString; cdecl; inline;
    function prepend_len(val: Pgchar; len: gssize): PGString; cdecl; inline;
    function prepend_unichar(wc: gunichar): PGString; cdecl; inline;
    //procedure printf(format: Pgchar; args: array of const); cdecl; inline;
    function set_size(len: gsize): PGString; cdecl; inline;
    function truncate(len: gsize): PGString; cdecl; inline;
    function up: PGString; cdecl; inline;
    //procedure vprintf(format: Pgchar; args: Tva_list); cdecl; inline;
  end;

  PPGIOError = ^PGIOError;
  PGIOError = ^TGIOError;

  PPGIOChannelError = ^PGIOChannelError;
  PGIOChannelError = ^TGIOChannelError;
  TGIOChannel = object
    ref_count: gint;
    funcs: PGIOFuncs;
    encoding: Pgchar;
    read_cd: TGIConv;
    write_cd: TGIConv;
    line_term: Pgchar;
    line_term_len: guint;
    buf_size: gsize;
    read_buf: PGString;
    encoded_read_buf: PGString;
    write_buf: PGString;
    partial_write_buf: array [0..5] of gchar;
    use_buffer: guint1 { changed from guint to accomodate 1 bitsize requirement };
    do_encode: guint1 { changed from guint to accomodate 1 bitsize requirement };
    close_on_unref: guint1 { changed from guint to accomodate 1 bitsize requirement };
    is_readable: guint1 { changed from guint to accomodate 1 bitsize requirement };
    is_writeable: guint1 { changed from guint to accomodate 1 bitsize requirement };
    is_seekable: guint1 { changed from guint to accomodate 1 bitsize requirement };
    reserved1: gpointer;
    reserved2: gpointer;
    function new_file(filename: Pgchar; mode: Pgchar): PGIOChannel; cdecl; inline; static;
    function unix_new(fd: gint): PGIOChannel; cdecl; inline; static;
    procedure close; cdecl; inline;
    function flush: TGIOStatus; cdecl; inline;
    function get_buffer_condition: TGIOCondition; cdecl; inline;
    function get_buffer_size: gsize; cdecl; inline;
    function get_buffered: gboolean; cdecl; inline;
    function get_close_on_unref: gboolean; cdecl; inline;
    function get_encoding: Pgchar; cdecl; inline;
    function get_flags: TGIOFlags; cdecl; inline;
    function get_line_term(length: Pgint): Pgchar; cdecl; inline;
    procedure init; cdecl; inline;
    function read(buf: Pgchar; count: gsize; bytes_read: Pgsize): TGIOError; cdecl; inline;
    function read_chars(buf: Pgchar; count: gsize; bytes_read: Pgsize): TGIOStatus; cdecl; inline;
    function read_line(str_return: PPgchar; length: Pgsize; terminator_pos: Pgsize): TGIOStatus; cdecl; inline;
    function read_line_string(buffer: PGString; terminator_pos: Pgsize): TGIOStatus; cdecl; inline;
    function read_to_end(str_return: PPgchar; length: Pgsize): TGIOStatus; cdecl; inline;
    function read_unichar(thechar: Pgunichar): TGIOStatus; cdecl; inline;
    function ref: PGIOChannel; cdecl; inline;
    function seek(offset: gint64; type_: TGSeekType): TGIOError; cdecl; inline;
    function seek_position(offset: gint64; type_: TGSeekType): TGIOStatus; cdecl; inline;
    procedure set_buffer_size(size: gsize); cdecl; inline;
    procedure set_buffered(buffered: gboolean); cdecl; inline;
    procedure set_close_on_unref(do_close: gboolean); cdecl; inline;
    function set_encoding(encoding: Pgchar): TGIOStatus; cdecl; inline;
    function set_flags(flags: TGIOFlags): TGIOStatus; cdecl; inline;
    procedure set_line_term(line_term: Pgchar; length: gint); cdecl; inline;
    function shutdown(flush: gboolean): TGIOStatus; cdecl; inline;
    function unix_get_fd: gint; cdecl; inline;
    procedure unref; cdecl; inline;
    function write(buf: Pgchar; count: gsize; bytes_written: Pgsize): TGIOError; cdecl; inline;
    function write_chars(buf: Pgchar; count: gssize; bytes_written: Pgsize): TGIOStatus; cdecl; inline;
    function write_unichar(thechar: gunichar): TGIOStatus; cdecl; inline;
    function error_from_errno(en: gint): TGIOChannelError; cdecl; inline; static;
    function error_quark: TGQuark; cdecl; inline; static;
  end;
  TGIOFunc = function(source: PGIOChannel; condition: TGIOCondition; data: gpointer): gboolean; cdecl;
  TGKeyFileFlags = packed object(TBitObject32)
  public
    property none: DWord index 0 read GetBit write SetBit;
    property keep_comments: DWord index 1 read GetBit write SetBit;
    property keep_translations: DWord index 2 read GetBit write SetBit;
  end;


  PPGKeyFile = ^PGKeyFile;
  PGKeyFile = ^TGKeyFile;

  PPguint64 = ^Pguint64;
  Pguint64 = ^guint64;

  PPGKeyFileFlags = ^PGKeyFileFlags;
  PGKeyFileFlags = ^TGKeyFileFlags;
  TGKeyFile = object
    procedure free; cdecl; inline;
    function get_boolean(group_name: Pgchar; key: Pgchar): gboolean; cdecl; inline;
    function get_boolean_list(group_name: Pgchar; key: Pgchar; length: Pgsize): Pgboolean; cdecl; inline;
    function get_comment(group_name: Pgchar; key: Pgchar): Pgchar; cdecl; inline;
    function get_double(group_name: Pgchar; key: Pgchar): gdouble; cdecl; inline;
    function get_double_list(group_name: Pgchar; key: Pgchar; length: Pgsize): Pgdouble; cdecl; inline;
    function get_groups(length: Pgsize): PPgchar; cdecl; inline;
    function get_int64(group_name: Pgchar; key: Pgchar): gint64; cdecl; inline;
    function get_integer(group_name: Pgchar; key: Pgchar): gint; cdecl; inline;
    function get_integer_list(group_name: Pgchar; key: Pgchar; length: Pgsize): Pgint; cdecl; inline;
    function get_keys(group_name: Pgchar; length: Pgsize): PPgchar; cdecl; inline;
    function get_locale_string(group_name: Pgchar; key: Pgchar; locale: Pgchar): Pgchar; cdecl; inline;
    function get_locale_string_list(group_name: Pgchar; key: Pgchar; locale: Pgchar; length: Pgsize): PPgchar; cdecl; inline;
    function get_start_group: Pgchar; cdecl; inline;
    function get_string(group_name: Pgchar; key: Pgchar): Pgchar; cdecl; inline;
    function get_string_list(group_name: Pgchar; key: Pgchar; length: Pgsize): PPgchar; cdecl; inline;
    function get_uint64(group_name: Pgchar; key: Pgchar): guint64; cdecl; inline;
    function get_value(group_name: Pgchar; key: Pgchar): Pgchar; cdecl; inline;
    function has_group(group_name: Pgchar): gboolean; cdecl; inline;
    function has_key(group_name: Pgchar; key: Pgchar): gboolean; cdecl; inline;
    function load_from_data(data: Pgchar; length: gsize; flags: TGKeyFileFlags): gboolean; cdecl; inline;
    function load_from_data_dirs(file_: Pgchar; full_path: PPgchar; flags: TGKeyFileFlags): gboolean; cdecl; inline;
    function load_from_dirs(file_: Pgchar; search_dirs: PPgchar; full_path: PPgchar; flags: TGKeyFileFlags): gboolean; cdecl; inline;
    function load_from_file(file_: Pgchar; flags: TGKeyFileFlags): gboolean; cdecl; inline;
    function remove_comment(group_name: Pgchar; key: Pgchar): gboolean; cdecl; inline;
    function remove_group(group_name: Pgchar): gboolean; cdecl; inline;
    function remove_key(group_name: Pgchar; key: Pgchar): gboolean; cdecl; inline;
    procedure set_boolean(group_name: Pgchar; key: Pgchar; value: gboolean); cdecl; inline;
    procedure set_boolean_list(group_name: Pgchar; key: Pgchar; list: gboolean; length: gsize); cdecl; inline;
    function set_comment(group_name: Pgchar; key: Pgchar; comment: Pgchar): gboolean; cdecl; inline;
    procedure set_double(group_name: Pgchar; key: Pgchar; value: gdouble); cdecl; inline;
    procedure set_double_list(group_name: Pgchar; key: Pgchar; list: gdouble; length: gsize); cdecl; inline;
    procedure set_int64(group_name: Pgchar; key: Pgchar; value: gint64); cdecl; inline;
    procedure set_integer(group_name: Pgchar; key: Pgchar; value: gint); cdecl; inline;
    procedure set_integer_list(group_name: Pgchar; key: Pgchar; list: gint; length: gsize); cdecl; inline;
    procedure set_list_separator(separator: gchar); cdecl; inline;
    procedure set_locale_string(group_name: Pgchar; key: Pgchar; locale: Pgchar; string_: Pgchar); cdecl; inline;
    procedure set_locale_string_list(group_name: Pgchar; key: Pgchar; locale: Pgchar; list: Pgchar; length: gsize); cdecl; inline;
    procedure set_string(group_name: Pgchar; key: Pgchar; string_: Pgchar); cdecl; inline;
    procedure set_string_list(group_name: Pgchar; key: Pgchar; list: Pgchar; length: gsize); cdecl; inline;
    procedure set_uint64(group_name: Pgchar; key: Pgchar; value: guint64); cdecl; inline;
    procedure set_value(group_name: Pgchar; key: Pgchar; value: Pgchar); cdecl; inline;
    function to_data(length: Pgsize): Pgchar; cdecl; inline;
    function error_quark: TGQuark; cdecl; inline; static;
    function new: PGKeyFile; cdecl; inline; static;
  end;

  PPGKeyFileError = ^PGKeyFileError;
  PGKeyFileError = ^TGKeyFileError;
  TGLogLevelFlags = packed object(TBitObject32)
  public
    property flag_recursion: DWord index 1 read GetBit write SetBit;
    property flag_fatal: DWord index 2 read GetBit write SetBit;
    property level_error: DWord index 4 read GetBit write SetBit;
    property level_critical: DWord index 8 read GetBit write SetBit;
    property level_warning: DWord index 16 read GetBit write SetBit;
    property level_message: DWord index 32 read GetBit write SetBit;
    property level_info: DWord index 64 read GetBit write SetBit;
    property level_debug: DWord index 128 read GetBit write SetBit;
    property level_mask: DWord index -4 read GetBit write SetBit;
  end;


  PPGLogLevelFlags = ^PGLogLevelFlags;
  PGLogLevelFlags = ^TGLogLevelFlags;
  TGLogFunc = procedure(log_domain: Pgchar; log_level: TGLogLevelFlags; message: Pgchar; user_data: gpointer); cdecl;

  PPgushort = ^Pgushort;
  Pgushort = ^gushort;

  TGPollFD = record
    fd: gint;
    events: gushort;
    revents: gushort;
  end;



  PPGSourceDummyMarshal = ^PGSourceDummyMarshal;
  PGSourceDummyMarshal = ^TGSourceDummyMarshal;
  TGSourceDummyMarshal = procedure; cdecl;

  TGSourceFuncs = record
    prepare: function(source: PGSource; timeout_: Pgint): gboolean; cdecl;
    check: function(source: PGSource): gboolean; cdecl;
    dispatch: function(source: PGSource; callback: TGSourceFunc; user_data: gpointer): gboolean; cdecl;
    finalize: procedure(source: PGSource); cdecl;
    closure_callback: TGSourceFunc;
    closure_marshal: TGSourceDummyMarshal;
  end;


  TGPollFunc = function(ufds: PGPollFD; nfsd: guint; timeout_: gint): gint; cdecl;

  PPGPollFunc = ^PGPollFunc;
  PGPollFunc = ^TGPollFunc;

  PPGMutex = ^PGMutex;
  PGMutex = ^TGMutex;
  TGMainContext = object
    function acquire: gboolean; cdecl; inline;
    procedure add_poll(fd: PGPollFD; priority: gint); cdecl; inline;
    function check(max_priority: gint; fds: PGPollFD; n_fds: gint): gint; cdecl; inline;
    procedure dispatch; cdecl; inline;
    function find_source_by_funcs_user_data(funcs: PGSourceFuncs; user_data: gpointer): PGSource; cdecl; inline;
    function find_source_by_id(source_id: guint): PGSource; cdecl; inline;
    function find_source_by_user_data(user_data: gpointer): PGSource; cdecl; inline;
    function get_poll_func: TGPollFunc; cdecl; inline;
    procedure invoke(function_: TGSourceFunc; data: gpointer); cdecl; inline;
    procedure invoke_full(priority: gint; function_: TGSourceFunc; data: gpointer; notify: TGDestroyNotify); cdecl; inline;
    function is_owner: gboolean; cdecl; inline;
    function iteration(may_block: gboolean): gboolean; cdecl; inline;
    function pending: gboolean; cdecl; inline;
    procedure pop_thread_default; cdecl; inline;
    function prepare(priority: Pgint): gboolean; cdecl; inline;
    procedure push_thread_default; cdecl; inline;
    function query(max_priority: gint; timeout_: Pgint; fds: PGPollFD; n_fds: gint): gint; cdecl; inline;
    function ref: PGMainContext; cdecl; inline;
    procedure release; cdecl; inline;
    procedure remove_poll(fd: PGPollFD); cdecl; inline;
    procedure set_poll_func(func: TGPollFunc); cdecl; inline;
    procedure unref; cdecl; inline;
    function wait(cond: PGCond; mutex: PGMutex): gboolean; cdecl; inline;
    procedure wakeup; cdecl; inline;
    function default_: PGMainContext; cdecl; inline; static;
    function get_thread_default: PGMainContext; cdecl; inline; static;
    function new: PGMainContext; cdecl; inline; static;
  end;

  TGMutex = record
    Unknown: Pointer;
  end;



  PPGMainLoop = ^PGMainLoop;
  PGMainLoop = ^TGMainLoop;
  TGMainLoop = object
    function get_context: PGMainContext; cdecl; inline;
    function is_running: gboolean; cdecl; inline;
    procedure quit; cdecl; inline;
    function ref: PGMainLoop; cdecl; inline;
    procedure run; cdecl; inline;
    procedure unref; cdecl; inline;
    function new(context: PGMainContext; is_running: gboolean): PGMainLoop; cdecl; inline; static;
  end;

  PPGMappedFile = ^PGMappedFile;
  PGMappedFile = ^TGMappedFile;
  TGMappedFile = object
    procedure free; cdecl; inline;
    function get_contents: Pgchar; cdecl; inline;
    function get_length: gsize; cdecl; inline;
    function ref: PGMappedFile; cdecl; inline;
    procedure unref; cdecl; inline;
    function new(filename: Pgchar; writable: gboolean): PGMappedFile; cdecl; inline; static;
  end;
  TGMarkupCollectType = packed object(TBitObject32)
  public
    property invalid: DWord index 0 read GetBit write SetBit;
    property string_: DWord index 1 read GetBit write SetBit;
    property strdup: DWord index 2 read GetBit write SetBit;
    property boolean_: DWord index 3 read GetBit write SetBit;
    property tristate: DWord index 4 read GetBit write SetBit;
    property optional: DWord index 65536 read GetBit write SetBit;
  end;


  PPGMarkupError = ^PGMarkupError;
  PGMarkupError = ^TGMarkupError;
  TGSList = object
    data: gpointer;
    next: PGSList;
    function alloc: PGSList; cdecl; inline; static;
    function append(list: PGSList; data: gpointer): PGSList; cdecl; inline; static;
    function concat(list1: PGSList; list2: PGSList): PGSList; cdecl; inline; static;
    function copy(list: PGSList): PGSList; cdecl; inline; static;
    function delete_link(list: PGSList; link_: PGSList): PGSList; cdecl; inline; static;
    function find(list: PGSList; data: gpointer): PGSList; cdecl; inline; static;
    function find_custom(list: PGSList; data: gpointer; func: TGCompareFunc): PGSList; cdecl; inline; static;
    procedure foreach(list: PGSList; func: TGFunc; user_data: gpointer); cdecl; inline; static;
    procedure free(list: PGSList); cdecl; inline; static;
    procedure free_1(list: PGSList); cdecl; inline; static;
    procedure free_full(list: PGSList; free_func: TGDestroyNotify); cdecl; inline; static;
    function index(list: PGSList; data: gpointer): gint; cdecl; inline; static;
    function insert(list: PGSList; data: gpointer; position: gint): PGSList; cdecl; inline; static;
    function insert_before(slist: PGSList; sibling: PGSList; data: gpointer): PGSList; cdecl; inline; static;
    function insert_sorted(list: PGSList; data: gpointer; func: TGCompareFunc): PGSList; cdecl; inline; static;
    function insert_sorted_with_data(list: PGSList; data: gpointer; func: TGCompareDataFunc; user_data: gpointer): PGSList; cdecl; inline; static;
    function last(list: PGSList): PGSList; cdecl; inline; static;
    function length(list: PGSList): guint; cdecl; inline; static;
    function nth(list: PGSList; n: guint): PGSList; cdecl; inline; static;
    function nth_data(list: PGSList; n: guint): gpointer; cdecl; inline; static;
    
    function position(list: PGSList; llink: PGSList): gint; cdecl; inline; static;
    function prepend(list: PGSList; data: gpointer): PGSList; cdecl; inline; static;
    
    function remove(list: PGSList; data: gpointer): PGSList; cdecl; inline; static;
    function remove_all(list: PGSList; data: gpointer): PGSList; cdecl; inline; static;
    function remove_link(list: PGSList; link_: PGSList): PGSList; cdecl; inline; static;
    function reverse(list: PGSList): PGSList; cdecl; inline; static;
    function sort(list: PGSList; compare_func: TGCompareFunc): PGSList; cdecl; inline; static;
    function sort_with_data(list: PGSList; compare_func: TGCompareDataFunc; user_data: gpointer): PGSList; cdecl; inline; static;
  end;

  PPGMarkupParser = ^PGMarkupParser;
  PGMarkupParser = ^TGMarkupParser;

  PPGMarkupParseContext = ^PGMarkupParseContext;
  PGMarkupParseContext = ^TGMarkupParseContext;

  TGMarkupParser = record
    start_element: procedure(context: PGMarkupParseContext; element_name: Pgchar; attribute_names: PPgchar; attribute_values: PPgchar; user_data: gpointer); cdecl;
    end_element: procedure(context: PGMarkupParseContext; element_name: Pgchar; user_data: gpointer); cdecl;
    text: procedure(context: PGMarkupParseContext; text: Pgchar; text_len: gsize; user_data: gpointer); cdecl;
    passthrough: procedure(context: PGMarkupParseContext; passthrough_text: Pgchar; text_len: gsize; user_data: gpointer); cdecl;
    error: procedure(context: PGMarkupParseContext; error: PGError; user_data: gpointer); cdecl;
  end;



  PPGMarkupParseFlags = ^PGMarkupParseFlags;
  PGMarkupParseFlags = ^TGMarkupParseFlags;
  TGMarkupParseFlags = packed object(TBitObject32)
  public
    property do_not_use_this_unsupported_flag: DWord index 1 read GetBit write SetBit;
    property treat_cdata_as_text: DWord index 2 read GetBit write SetBit;
    property prefix_error_position: DWord index 4 read GetBit write SetBit;
  end;

  TGMarkupParseContext = object
    function end_parse: gboolean; cdecl; inline;
    procedure free; cdecl; inline;
    function get_element: Pgchar; cdecl; inline;
    function get_element_stack: PGSList; cdecl; inline;
    procedure get_position(line_number: Pgint; char_number: Pgint); cdecl; inline;
    function get_user_data: gpointer; cdecl; inline;
    function parse(text: Pgchar; text_len: gssize): gboolean; cdecl; inline;
    function pop: gpointer; cdecl; inline;
    procedure push(parser: PGMarkupParser; user_data: gpointer); cdecl; inline;
    function new(parser: PGMarkupParser; flags: TGMarkupParseFlags; user_data: gpointer; user_data_dnotify: TGDestroyNotify): PGMarkupParseContext; cdecl; inline; static;
  end;

  PPGRegex = ^PGRegex;
  PGRegex = ^TGRegex;

  PPGRegexCompileFlags = ^PGRegexCompileFlags;
  PGRegexCompileFlags = ^TGRegexCompileFlags;
  TGRegexCompileFlags = packed object(TBitObject32)
  public
    property caseless: DWord index 1 read GetBit write SetBit;
    property multiline: DWord index 2 read GetBit write SetBit;
    property dotall: DWord index 4 read GetBit write SetBit;
    property extended: DWord index 8 read GetBit write SetBit;
    property anchored: DWord index 16 read GetBit write SetBit;
    property dollar_endonly: DWord index 32 read GetBit write SetBit;
    property ungreedy: DWord index 512 read GetBit write SetBit;
    property raw: DWord index 2048 read GetBit write SetBit;
    property no_auto_capture: DWord index 4096 read GetBit write SetBit;
    property optimize: DWord index 8192 read GetBit write SetBit;
    property dupnames: DWord index 524288 read GetBit write SetBit;
    property newline_cr: DWord index 1048576 read GetBit write SetBit;
    property newline_lf: DWord index 2097152 read GetBit write SetBit;
    property newline_crlf: DWord index 3145728 read GetBit write SetBit;
  end;


  PPGRegexMatchFlags = ^PGRegexMatchFlags;
  PGRegexMatchFlags = ^TGRegexMatchFlags;
  TGRegexMatchFlags = packed object(TBitObject32)
  public
    property anchored: DWord index 16 read GetBit write SetBit;
    property notbol: DWord index 128 read GetBit write SetBit;
    property noteol: DWord index 256 read GetBit write SetBit;
    property notempty: DWord index 1024 read GetBit write SetBit;
    property partial: DWord index 32768 read GetBit write SetBit;
    property newline_cr: DWord index 1048576 read GetBit write SetBit;
    property newline_lf: DWord index 2097152 read GetBit write SetBit;
    property newline_crlf: DWord index 3145728 read GetBit write SetBit;
    property newline_any: DWord index 4194304 read GetBit write SetBit;
  end;


  PPGMatchInfo = ^PGMatchInfo;
  PGMatchInfo = ^TGMatchInfo;

  PPGRegexEvalCallback = ^PGRegexEvalCallback;
  PGRegexEvalCallback = ^TGRegexEvalCallback;
  TGRegexEvalCallback = function(match_info: PGMatchInfo; result_: PGString; user_data: gpointer): gboolean; cdecl;
  TGRegex = object
    function new(pattern: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): PGRegex; cdecl; inline; static;
    function get_capture_count: gint; cdecl; inline;
    function get_compile_flags: TGRegexCompileFlags; cdecl; inline;
    function get_match_flags: TGRegexMatchFlags; cdecl; inline;
    function get_max_backref: gint; cdecl; inline;
    function get_pattern: Pgchar; cdecl; inline;
    function get_string_number(name: Pgchar): gint; cdecl; inline;
    function match(string_: Pgchar; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl; inline;
    function match_all(string_: Pgchar; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl; inline;
    function match_all_full(string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl; inline;
    function match_full(string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl; inline;
    function ref: PGRegex; cdecl; inline;
    function replace(string_: Pgchar; string_len: gssize; start_position: gint; replacement: Pgchar; match_options: TGRegexMatchFlags): Pgchar; cdecl; inline;
    function replace_eval(string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; eval: TGRegexEvalCallback; user_data: gpointer): Pgchar; cdecl; inline;
    function replace_literal(string_: Pgchar; string_len: gssize; start_position: gint; replacement: Pgchar; match_options: TGRegexMatchFlags): Pgchar; cdecl; inline;
    function split(string_: Pgchar; match_options: TGRegexMatchFlags): PPgchar; cdecl; inline;
    function split_full(string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; max_tokens: gint): PPgchar; cdecl; inline;
    procedure unref; cdecl; inline;
    function check_replacement(replacement: Pgchar; has_references: Pgboolean): gboolean; cdecl; inline; static;
    function error_quark: TGQuark; cdecl; inline; static;
    function escape_nul(string_: Pgchar; length: gint): Pgchar; cdecl; inline; static;
    function escape_string(string_: Pgchar; length: gint): Pgchar; cdecl; inline; static;
    function match_simple(pattern: Pgchar; string_: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): gboolean; cdecl; inline; static;
    function split_simple(pattern: Pgchar; string_: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): PPgchar; cdecl; inline; static;
  end;
  TGMatchInfo = object
    function expand_references(string_to_expand: Pgchar): Pgchar; cdecl; inline;
    function fetch(match_num: gint): Pgchar; cdecl; inline;
    function fetch_all: PPgchar; cdecl; inline;
    function fetch_named(name: Pgchar): Pgchar; cdecl; inline;
    function fetch_named_pos(name: Pgchar; start_pos: Pgint; end_pos: Pgint): gboolean; cdecl; inline;
    function fetch_pos(match_num: gint; start_pos: Pgint; end_pos: Pgint): gboolean; cdecl; inline;
    procedure free; cdecl; inline;
    function get_match_count: gint; cdecl; inline;
    function get_regex: PGRegex; cdecl; inline;
    function get_string: Pgchar; cdecl; inline;
    function is_partial_match: gboolean; cdecl; inline;
    function matches: gboolean; cdecl; inline;
    function next: gboolean; cdecl; inline;
    function ref: PGMatchInfo; cdecl; inline;
    procedure unref; cdecl; inline;
  end;

  PPGMemChunk = ^PGMemChunk;
  PGMemChunk = ^TGMemChunk;
  TGMemChunk = object
    function alloc: gpointer; cdecl; inline;
    function alloc0: gpointer; cdecl; inline;
    procedure clean; cdecl; inline;
    procedure destroy_; cdecl; inline;
    procedure free(mem: gpointer); cdecl; inline;
    procedure print; cdecl; inline;
    procedure reset; cdecl; inline;
    procedure info; cdecl; inline; static;
    function new(name: Pgchar; atom_size: gint; area_size: gsize; type_: gint): PGMemChunk; cdecl; inline; static;
  end;

  PPGMemVTable = ^PGMemVTable;
  PGMemVTable = ^TGMemVTable;

  TGMemVTable = record
    malloc: function(n_bytes: gsize): gpointer; cdecl;
    realloc: function(mem: gpointer; n_bytes: gsize): gpointer; cdecl;
    free: procedure(mem: gpointer); cdecl;
    calloc: function(n_blocks: gsize; n_block_bytes: gsize): gpointer; cdecl;
    try_malloc: function(n_bytes: gsize): gpointer; cdecl;
    try_realloc: function(mem: gpointer; n_bytes: gsize): gpointer; cdecl;
  end;



  PPGNode = ^PGNode;
  PGNode = ^TGNode;

  PPGTraverseFlags = ^PGTraverseFlags;
  PGTraverseFlags = ^TGTraverseFlags;
  TGTraverseFlags = packed object(TBitObject32)
  public
    property leaves: DWord index 1 read GetBit write SetBit;
    property non_leaves: DWord index 2 read GetBit write SetBit;
    property all: DWord index 3 read GetBit write SetBit;
    property mask: DWord index 3 read GetBit write SetBit;
    property leafs: DWord index 1 read GetBit write SetBit;
    property non_leafs: DWord index 2 read GetBit write SetBit;
  end;


  PPGNodeForeachFunc = ^PGNodeForeachFunc;
  PGNodeForeachFunc = ^TGNodeForeachFunc;
  TGNodeForeachFunc = procedure(node: PGNode; data: gpointer); cdecl;

  PPGCopyFunc = ^PGCopyFunc;
  PGCopyFunc = ^TGCopyFunc;

  PPGTraverseType = ^PGTraverseType;
  PGTraverseType = ^TGTraverseType;

  PPGNodeTraverseFunc = ^PGNodeTraverseFunc;
  PGNodeTraverseFunc = ^TGNodeTraverseFunc;
  TGNodeTraverseFunc = function(node: PGNode; data: gpointer): gboolean; cdecl;
  TGNode = object
    data: gpointer;
    next: PGNode;
    prev: PGNode;
    parent: PGNode;
    children: PGNode;
    function child_index(data: gpointer): gint; cdecl; inline;
    function child_position(child: PGNode): gint; cdecl; inline;
    procedure children_foreach(flags: TGTraverseFlags; func: TGNodeForeachFunc; data: gpointer); cdecl; inline;
    function copy: PGNode; cdecl; inline;
    function copy_deep(copy_func: TGCopyFunc; data: gpointer): PGNode; cdecl; inline;
    function depth: guint; cdecl; inline;
    procedure destroy_; cdecl; inline;
    function find(order: TGTraverseType; flags: TGTraverseFlags; data: gpointer): PGNode; cdecl; inline;
    function find_child(flags: TGTraverseFlags; data: gpointer): PGNode; cdecl; inline;
    function first_sibling: PGNode; cdecl; inline;
    function get_root: PGNode; cdecl; inline;
    function insert(position: gint; node: PGNode): PGNode; cdecl; inline;
    function insert_after(sibling: PGNode; node: PGNode): PGNode; cdecl; inline;
    function insert_before(sibling: PGNode; node: PGNode): PGNode; cdecl; inline;
    function is_ancestor(descendant: PGNode): gboolean; cdecl; inline;
    function last_child: PGNode; cdecl; inline;
    function last_sibling: PGNode; cdecl; inline;
    function max_height: guint; cdecl; inline;
    function n_children: guint; cdecl; inline;
    function n_nodes(flags: TGTraverseFlags): guint; cdecl; inline;
    function nth_child(n: guint): PGNode; cdecl; inline;
    function prepend(node: PGNode): PGNode; cdecl; inline;
    procedure reverse_children; cdecl; inline;
    procedure traverse(order: TGTraverseType; flags: TGTraverseFlags; max_depth: gint; func: TGNodeTraverseFunc; data: gpointer); cdecl; inline;
    procedure unlink; cdecl; inline;
    function new(data: gpointer): PGNode; cdecl; inline; static;
    procedure pop_allocator; cdecl; inline; static;
    procedure push_allocator(dummy: gpointer); cdecl; inline; static;
  end;

  PPGNormalizeMode = ^PGNormalizeMode;
  PGNormalizeMode = ^TGNormalizeMode;

  PPGOnceStatus = ^PGOnceStatus;
  PGOnceStatus = ^TGOnceStatus;
  TGThreadFunc = function(data: gpointer): gpointer; cdecl;

  PPGOnce = ^PGOnce;
  PGOnce = ^TGOnce;

  PPGThreadFunc = ^PGThreadFunc;
  PGThreadFunc = ^TGThreadFunc;
  TGOnce = object
    status: TGOnceStatus;
    retval: gpointer;
    function impl(func: TGThreadFunc; arg: gpointer): gpointer; cdecl; inline;
    function init_enter(value_location: Pgsize): gboolean; cdecl; inline; static;
    function init_enter_impl(value_location: Pgsize): gboolean; cdecl; inline; static;
    procedure init_leave(value_location: Pgsize; initialization_value: gsize); cdecl; inline; static;
  end;

  PPGOptionArg = ^PGOptionArg;
  PGOptionArg = ^TGOptionArg;
  TGOptionArgFunc = function(option_name: Pgchar; value: Pgchar; data: gpointer): gboolean; cdecl;

  PPGOptionGroup = ^PGOptionGroup;
  PGOptionGroup = ^TGOptionGroup;

  PPGOptionEntry = ^PGOptionEntry;
  PGOptionEntry = ^TGOptionEntry;

  PPGOptionErrorFunc = ^PGOptionErrorFunc;
  PGOptionErrorFunc = ^TGOptionErrorFunc;

  PPGOptionContext = ^PGOptionContext;
  PGOptionContext = ^TGOptionContext;
  TGOptionErrorFunc = procedure(context: PGOptionContext; group: PGOptionGroup; data: gpointer); cdecl;

  PPGOptionParseFunc = ^PGOptionParseFunc;
  PGOptionParseFunc = ^TGOptionParseFunc;
  TGOptionParseFunc = function(context: PGOptionContext; group: PGOptionGroup; data: gpointer): gboolean; cdecl;

  PPGTranslateFunc = ^PGTranslateFunc;
  PGTranslateFunc = ^TGTranslateFunc;
  TGTranslateFunc = function(str: Pgchar; data: gpointer): Pgchar; cdecl;
  TGOptionGroup = object
    procedure add_entries(entries: PGOptionEntry); cdecl; inline;
    procedure free; cdecl; inline;
    procedure set_error_hook(error_func: TGOptionErrorFunc); cdecl; inline;
    procedure set_parse_hooks(pre_parse_func: TGOptionParseFunc; post_parse_func: TGOptionParseFunc); cdecl; inline;
    procedure set_translate_func(func: TGTranslateFunc; data: gpointer; destroy_notify: TGDestroyNotify); cdecl; inline;
    procedure set_translation_domain(domain: Pgchar); cdecl; inline;
    function new(name: Pgchar; description: Pgchar; help_description: Pgchar; user_data: gpointer; destroy_: TGDestroyNotify): PGOptionGroup; cdecl; inline; static;
  end;

  TGOptionEntry = record
    long_name: Pgchar;
    short_name: gchar;
    flags: gint;
    arg: TGOptionArg;
    arg_data: gpointer;
    description: Pgchar;
    arg_description: Pgchar;
  end;


  TGOptionContext = object
    procedure add_group(group: PGOptionGroup); cdecl; inline;
    procedure add_main_entries(entries: PGOptionEntry; translation_domain: Pgchar); cdecl; inline;
    procedure free; cdecl; inline;
    function get_description: Pgchar; cdecl; inline;
    function get_help(main_help: gboolean; group: PGOptionGroup): Pgchar; cdecl; inline;
    function get_help_enabled: gboolean; cdecl; inline;
    function get_ignore_unknown_options: gboolean; cdecl; inline;
    function get_main_group: PGOptionGroup; cdecl; inline;
    function get_summary: Pgchar; cdecl; inline;
    function parse(argc: Pgint; argv: PPPgchar): gboolean; cdecl; inline;
    procedure set_description(description: Pgchar); cdecl; inline;
    procedure set_help_enabled(help_enabled: gboolean); cdecl; inline;
    procedure set_ignore_unknown_options(ignore_unknown: gboolean); cdecl; inline;
    procedure set_main_group(group: PGOptionGroup); cdecl; inline;
    procedure set_summary(summary: Pgchar); cdecl; inline;
    procedure set_translate_func(func: TGTranslateFunc; data: gpointer; destroy_notify: TGDestroyNotify); cdecl; inline;
    procedure set_translation_domain(domain: Pgchar); cdecl; inline;
    function new(parameter_string: Pgchar): PGOptionContext; cdecl; inline; static;
  end;

  PPGOptionError = ^PGOptionError;
  PGOptionError = ^TGOptionError;
  TGOptionFlags = packed object(TBitObject32)
  public
    property hidden: DWord index 1 read GetBit write SetBit;
    property in_main: DWord index 2 read GetBit write SetBit;
    property reverse: DWord index 4 read GetBit write SetBit;
    property no_arg: DWord index 8 read GetBit write SetBit;
    property filename: DWord index 16 read GetBit write SetBit;
    property optional_arg: DWord index 32 read GetBit write SetBit;
    property noalias: DWord index 64 read GetBit write SetBit;
  end;


  PPGPatternSpec = ^PGPatternSpec;
  PGPatternSpec = ^TGPatternSpec;
  TGPatternSpec = object
    function equal(pspec2: PGPatternSpec): gboolean; cdecl; inline;
    procedure free; cdecl; inline;
    function new(pattern: Pgchar): PGPatternSpec; cdecl; inline; static;
  end;
  TGPrintFunc = procedure(string_: Pgchar); cdecl;

  PPGPrivate = ^PGPrivate;
  PGPrivate = ^TGPrivate;

  TGPrivate = record
    Unknown: Pointer;
  end;



  PPGPtrArray = ^PGPtrArray;
  PGPtrArray = ^TGPtrArray;
  TGPtrArray = object
    pdata: Pgpointer;
    len: guint;
    procedure add(array_: Pgpointer; data: gpointer); cdecl; inline; static;
    procedure foreach(array_: Pgpointer; func: TGFunc; user_data: gpointer); cdecl; inline; static;
    function free(array_: Pgpointer; free_seg: gboolean): Pgpointer; cdecl; inline; static;
    function new: Pgpointer; cdecl; inline; static;
    function new_full(reserved_size: guint; element_free_func: TGDestroyNotify): Pgpointer; cdecl; inline; static;
    function new_with_free_func(element_free_func: TGDestroyNotify): Pgpointer; cdecl; inline; static;
    function ref(array_: Pgpointer): Pgpointer; cdecl; inline; static;
    function remove(array_: Pgpointer; data: gpointer): gboolean; cdecl; inline; static;
    function remove_fast(array_: Pgpointer; data: gpointer): gboolean; cdecl; inline; static;
    function remove_index(array_: Pgpointer; index_: guint): gpointer; cdecl; inline; static;
    function remove_index_fast(array_: Pgpointer; index_: guint): gpointer; cdecl; inline; static;
    procedure remove_range(array_: Pgpointer; index_: guint; length: guint); cdecl; inline; static;
    procedure set_free_func(array_: Pgpointer; element_free_func: TGDestroyNotify); cdecl; inline; static;
    procedure set_size(array_: Pgpointer; length: gint); cdecl; inline; static;
    function sized_new(reserved_size: guint): Pgpointer; cdecl; inline; static;
    procedure sort(array_: Pgpointer; compare_func: TGCompareFunc); cdecl; inline; static;
    procedure sort_with_data(array_: Pgpointer; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; inline; static;
    procedure unref(array_: Pgpointer); cdecl; inline; static;
  end;

  PPGQueue = ^PGQueue;
  PGQueue = ^TGQueue;
  TGQueue = object
    head: PGList;
    tail: PGList;
    length: guint;
    procedure clear; cdecl; inline;
    function copy: PGQueue; cdecl; inline;
    procedure delete_link(link_: PGList); cdecl; inline;
    function find(data: gpointer): PGList; cdecl; inline;
    function find_custom(data: gpointer; func: TGCompareFunc): PGList; cdecl; inline;
    procedure foreach(func: TGFunc; user_data: gpointer); cdecl; inline;
    procedure free; cdecl; inline;
    function get_length: guint; cdecl; inline;
    function index(data: gpointer): gint; cdecl; inline;
    procedure init; cdecl; inline;
    procedure insert_after(sibling: PGList; data: gpointer); cdecl; inline;
    procedure insert_before(sibling: PGList; data: gpointer); cdecl; inline;
    procedure insert_sorted(data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl; inline;
    function is_empty: gboolean; cdecl; inline;
    function link_index(link_: PGList): gint; cdecl; inline;
    function peek_head: gpointer; cdecl; inline;
    function peek_head_link: PGList; cdecl; inline;
    function peek_nth(n: guint): gpointer; cdecl; inline;
    function peek_nth_link(n: guint): PGList; cdecl; inline;
    function peek_tail: gpointer; cdecl; inline;
    function peek_tail_link: PGList; cdecl; inline;
    function pop_head: gpointer; cdecl; inline;
    function pop_head_link: PGList; cdecl; inline;
    function pop_nth(n: guint): gpointer; cdecl; inline;
    function pop_nth_link(n: guint): PGList; cdecl; inline;
    function pop_tail: gpointer; cdecl; inline;
    function pop_tail_link: PGList; cdecl; inline;
    procedure push_head(data: gpointer); cdecl; inline;
    procedure push_head_link(link_: PGList); cdecl; inline;
    procedure push_nth(data: gpointer; n: gint); cdecl; inline;
    procedure push_nth_link(n: gint; link_: PGList); cdecl; inline;
    procedure push_tail(data: gpointer); cdecl; inline;
    procedure push_tail_link(link_: PGList); cdecl; inline;
    function remove(data: gpointer): gboolean; cdecl; inline;
    function remove_all(data: gpointer): guint; cdecl; inline;
    procedure reverse; cdecl; inline;
    procedure sort(compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; inline;
    procedure unlink(link_: PGList); cdecl; inline;
    function new: PGQueue; cdecl; inline; static;
  end;

  PPGRand = ^PGRand;
  PGRand = ^TGRand;
  TGRand = object
    function copy: PGRand; cdecl; inline;
    function double: gdouble; cdecl; inline;
    function double_range(begin_: gdouble; end_: gdouble): gdouble; cdecl; inline;
    procedure free; cdecl; inline;
    function int: guint32; cdecl; inline;
    function int_range(begin_: gint32; end_: gint32): gint32; cdecl; inline;
    procedure set_seed(seed: guint32); cdecl; inline;
    procedure set_seed_array(seed: Pguint32; seed_length: guint); cdecl; inline;
    function new: PGRand; cdecl; inline; static;
    function new_with_seed(seed: guint32): PGRand; cdecl; inline; static;
    function new_with_seed_array(seed: Pguint32; seed_length: guint): PGRand; cdecl; inline; static;
  end;

  PPGRegexError = ^PGRegexError;
  PGRegexError = ^TGRegexError;

  PPGTuples = ^PGTuples;
  PGTuples = ^TGTuples;
  TGTuples = object
    len: guint;
    
    
  end;

  PPGRelation = ^PGRelation;
  PGRelation = ^TGRelation;
  TGRelation = object
    
    
    
    
    
    
    
    
    
  end;

  PPGScannerConfig = ^PGScannerConfig;
  PGScannerConfig = ^TGScannerConfig;

  TGScannerConfig = record
    cset_skip_characters: Pgchar;
    cset_identifier_first: Pgchar;
    cset_identifier_nth: Pgchar;
    cpair_comment_single: Pgchar;
    case_sensitive: guint1 { changed from guint to accomodate 1 bitsize requirement };
    skip_comment_multi: guint1 { changed from guint to accomodate 1 bitsize requirement };
    skip_comment_single: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_comment_multi: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_identifier: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_identifier_1char: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_identifier_NULL: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_symbols: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_binary: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_octal: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_float: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_hex: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_hex_dollar: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_string_sq: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scan_string_dq: guint1 { changed from guint to accomodate 1 bitsize requirement };
    numbers_2_int: guint1 { changed from guint to accomodate 1 bitsize requirement };
    int_2_float: guint1 { changed from guint to accomodate 1 bitsize requirement };
    identifier_2_string: guint1 { changed from guint to accomodate 1 bitsize requirement };
    char_2_token: guint1 { changed from guint to accomodate 1 bitsize requirement };
    symbol_2_token: guint1 { changed from guint to accomodate 1 bitsize requirement };
    scope_0_fallback: guint1 { changed from guint to accomodate 1 bitsize requirement };
    store_int64: guint1 { changed from guint to accomodate 1 bitsize requirement };
    padding_dummy: guint;
  end;



  PPGTokenType = ^PGTokenType;
  PGTokenType = ^TGTokenType;
  TGTokenValue = record
    case longint of
      0 : (v_symbol: gpointer);
      1 : (v_identifier: Pgchar);
      2 : (v_binary: gulong);
      3 : (v_octal: gulong);
      4 : (v_int: gulong);
      5 : (v_int64: guint64);
      6 : (v_float: gdouble);
      7 : (v_hex: gulong);
      8 : (v_string: Pgchar);
      9 : (v_comment: Pgchar);
      10 : (v_char: guint8);
      11 : (v_error: guint);
  end;



  PPGScanner = ^PGScanner;
  PGScanner = ^TGScanner;
  TGScannerMsgFunc = procedure(scanner: PGScanner; message: Pgchar; error: gboolean); cdecl;

  PPGTokenValue = ^PGTokenValue;
  PGTokenValue = ^TGTokenValue;

  PPGScannerMsgFunc = ^PGScannerMsgFunc;
  PGScannerMsgFunc = ^TGScannerMsgFunc;
  TGScanner = object
    user_data: gpointer;
    max_parse_errors: guint;
    parse_errors: guint;
    input_name: Pgchar;
    qdata: PGData;
    config: PGScannerConfig;
    token: TGTokenType;
    value: TGTokenValue;
    line: guint;
    position: guint;
    next_token: TGTokenType;
    next_value: TGTokenValue;
    next_line: guint;
    next_position: guint;
    symbol_table: PGHashTable;
    input_fd: gint;
    text: Pgchar;
    text_end: Pgchar;
    buffer: Pgchar;
    scope_id: guint;
    msg_handler: TGScannerMsgFunc;
    function cur_line: guint; cdecl; inline;
    function cur_position: guint; cdecl; inline;
    function cur_token: TGTokenType; cdecl; inline;
    function cur_value: TGTokenValue; cdecl; inline;
    procedure destroy_; cdecl; inline;
    function eof: gboolean; cdecl; inline;
    //procedure error(format: Pgchar; args: array of const); cdecl; inline;
    function get_next_token: TGTokenType; cdecl; inline;
    procedure input_file(input_fd: gint); cdecl; inline;
    procedure input_text(text: Pgchar; text_len: guint); cdecl; inline;
    function lookup_symbol(symbol: Pgchar): gpointer; cdecl; inline;
    function peek_next_token: TGTokenType; cdecl; inline;
    procedure scope_add_symbol(scope_id: guint; symbol: Pgchar; value: gpointer); cdecl; inline;
    procedure scope_foreach_symbol(scope_id: guint; func: TGHFunc; user_data: gpointer); cdecl; inline;
    function scope_lookup_symbol(scope_id: guint; symbol: Pgchar): gpointer; cdecl; inline;
    procedure scope_remove_symbol(scope_id: guint; symbol: Pgchar); cdecl; inline;
    function set_scope(scope_id: guint): guint; cdecl; inline;
    procedure sync_file_offset; cdecl; inline;
    procedure unexp_token(expected_token: TGTokenType; identifier_spec: Pgchar; symbol_spec: Pgchar; symbol_name: Pgchar; message: Pgchar; is_error: gint); cdecl; inline;
    //procedure warn(format: Pgchar; args: array of const); cdecl; inline;
    function new(config_templ: PGScannerConfig): PGScanner; cdecl; inline; static;
  end;

  PPGSequenceIter = ^PGSequenceIter;
  PGSequenceIter = ^TGSequenceIter;

  PPGSequence = ^PGSequence;
  PGSequence = ^TGSequence;
  TGSequenceIter = object
    function compare(b: PGSequenceIter): gint; cdecl; inline;
    function get_position: gint; cdecl; inline;
    function get_sequence: PGSequence; cdecl; inline;
    function is_begin: gboolean; cdecl; inline;
    function is_end: gboolean; cdecl; inline;
    function move(delta: gint): PGSequenceIter; cdecl; inline;
    function next: PGSequenceIter; cdecl; inline;
    function prev: PGSequenceIter; cdecl; inline;
  end;
  TGSequenceIterCompareFunc = function(a: PGSequenceIter; b: PGSequenceIter; data: gpointer): gint; cdecl;

  PPGSequenceIterCompareFunc = ^PGSequenceIterCompareFunc;
  PGSequenceIterCompareFunc = ^TGSequenceIterCompareFunc;
  TGSequence = object
    function append(data: gpointer): PGSequenceIter; cdecl; inline;
    procedure foreach(func: TGFunc; user_data: gpointer); cdecl; inline;
    procedure free; cdecl; inline;
    function get_begin_iter: PGSequenceIter; cdecl; inline;
    function get_end_iter: PGSequenceIter; cdecl; inline;
    function get_iter_at_pos(pos: gint): PGSequenceIter; cdecl; inline;
    function get_length: gint; cdecl; inline;
    function insert_sorted(data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl; inline;
    function insert_sorted_iter(data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl; inline;
    function lookup(data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl; inline;
    function lookup_iter(data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl; inline;
    function prepend(data: gpointer): PGSequenceIter; cdecl; inline;
    function search(data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl; inline;
    function search_iter(data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl; inline;
    procedure sort(cmp_func: TGCompareDataFunc; cmp_data: gpointer); cdecl; inline;
    procedure sort_iter(cmp_func: TGSequenceIterCompareFunc; cmp_data: gpointer); cdecl; inline;
    procedure foreach_range(begin_: PGSequenceIter; end_: PGSequenceIter; func: TGFunc; user_data: gpointer); cdecl; inline; static;
    function get(iter: PGSequenceIter): gpointer; cdecl; inline; static;
    function insert_before(iter: PGSequenceIter; data: gpointer): PGSequenceIter; cdecl; inline; static;
    procedure move(src: PGSequenceIter; dest: PGSequenceIter); cdecl; inline; static;
    procedure move_range(dest: PGSequenceIter; begin_: PGSequenceIter; end_: PGSequenceIter); cdecl; inline; static;
    function new(data_destroy: TGDestroyNotify): PGSequence; cdecl; inline; static;
    function range_get_midpoint(begin_: PGSequenceIter; end_: PGSequenceIter): PGSequenceIter; cdecl; inline; static;
    procedure remove(iter: PGSequenceIter); cdecl; inline; static;
    procedure remove_range(begin_: PGSequenceIter; end_: PGSequenceIter); cdecl; inline; static;
    procedure set_(iter: PGSequenceIter; data: gpointer); cdecl; inline; static;
    procedure sort_changed(iter: PGSequenceIter; cmp_func: TGCompareDataFunc; cmp_data: gpointer); cdecl; inline; static;
    procedure sort_changed_iter(iter: PGSequenceIter; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer); cdecl; inline; static;
    procedure swap(a: PGSequenceIter; b: PGSequenceIter); cdecl; inline; static;
  end;

  PPGShellError = ^PGShellError;
  PGShellError = ^TGShellError;

  PPGSliceConfig = ^PGSliceConfig;
  PGSliceConfig = ^TGSliceConfig;

  TGSourceCallbackFuncs = record
    ref: procedure(cb_data: gpointer); cdecl;
    unref: procedure(cb_data: gpointer); cdecl;
    get: procedure(cb_data: gpointer; source: PGSource; func: PGSourceFunc; data: Pgpointer); cdecl;
  end;



  TGSourcePrivate = record
    Unknown: Pointer;
  end;


  TGSpawnChildSetupFunc = procedure(user_data: gpointer); cdecl;

  PPGSpawnError = ^PGSpawnError;
  PGSpawnError = ^TGSpawnError;
  TGSpawnFlags = packed object(TBitObject32)
  public
    property leave_descriptors_open: DWord index 1 read GetBit write SetBit;
    property do_not_reap_child: DWord index 2 read GetBit write SetBit;
    property search_path: DWord index 4 read GetBit write SetBit;
    property stdout_to_dev_null: DWord index 8 read GetBit write SetBit;
    property stderr_to_dev_null: DWord index 16 read GetBit write SetBit;
    property child_inherits_stdin: DWord index 32 read GetBit write SetBit;
    property file_and_argv_zero: DWord index 64 read GetBit write SetBit;
  end;


  PPGStatBuf = ^PGStatBuf;
  PGStatBuf = ^TGStatBuf;

  TGStatBuf = record
    Unknown: Pointer;
  end;



  PPGStaticMutex = ^PGStaticMutex;
  PGStaticMutex = ^TGStaticMutex;
  TGStaticMutex_union_static_mutex = record
    case longint of
      0 : (pad: array [0..39] of gchar);
      1 : (dummy_double: gdouble);
      2 : (dummy_pointer: Pgpointer);
      3 : (dummy_long: glong);
  end;


  TGStaticMutex = object
    runtime_mutex: Pgpointer;
    static_mutex: TGStaticMutex_union_static_mutex; //union extracted from object and named 'TGStaticMutex_union_static_mutex'
    procedure free; cdecl; inline;
    procedure init; cdecl; inline;
    function get_mutex_impl(mutex: PPGMutex): PGMutex; cdecl; inline; static;
  end;

  PPGStaticPrivate = ^PGStaticPrivate;
  PGStaticPrivate = ^TGStaticPrivate;
  TGStaticPrivate = object
    index: guint;
    procedure free; cdecl; inline;
    function get: gpointer; cdecl; inline;
    procedure init; cdecl; inline;
    procedure set_(data: gpointer; notify: TGDestroyNotify); cdecl; inline;
  end;

  PPGStaticRWLock = ^PGStaticRWLock;
  PGStaticRWLock = ^TGStaticRWLock;
  TGStaticRWLock = object
    mutex: TGStaticMutex;
    read_cond: PGCond;
    write_cond: PGCond;
    read_counter: guint;
    have_writer: gboolean;
    want_to_read: guint;
    want_to_write: guint;
    procedure free; cdecl; inline;
    procedure init; cdecl; inline;
    procedure reader_lock; cdecl; inline;
    function reader_trylock: gboolean; cdecl; inline;
    procedure reader_unlock; cdecl; inline;
    procedure writer_lock; cdecl; inline;
    function writer_trylock: gboolean; cdecl; inline;
    procedure writer_unlock; cdecl; inline;
  end;
  TGSystemThread = record
    case longint of
      0 : (data: array [0..7] of gchar);
      1 : (dummy_double: gdouble);
      2 : (dummy_pointer: Pgpointer);
      3 : (dummy_long: glong);
  end;



  PPGStaticRecMutex = ^PGStaticRecMutex;
  PGStaticRecMutex = ^TGStaticRecMutex;

  PPGSystemThread = ^PGSystemThread;
  PGSystemThread = ^TGSystemThread;
  TGStaticRecMutex = object
    mutex: TGStaticMutex;
    depth: guint;
    owner: TGSystemThread;
    procedure free; cdecl; inline;
    procedure init; cdecl; inline;
    procedure lock; cdecl; inline;
    procedure lock_full(depth: guint); cdecl; inline;
    function trylock: gboolean; cdecl; inline;
    procedure unlock; cdecl; inline;
    function unlock_full: guint; cdecl; inline;
  end;

  PPGStringChunk = ^PGStringChunk;
  PGStringChunk = ^TGStringChunk;
  TGStringChunk = object
    procedure clear; cdecl; inline;
    procedure free; cdecl; inline;
    function insert(string_: Pgchar): Pgchar; cdecl; inline;
    function insert_const(string_: Pgchar): Pgchar; cdecl; inline;
    function insert_len(string_: Pgchar; len: gssize): Pgchar; cdecl; inline;
    function new(size: gsize): PGStringChunk; cdecl; inline; static;
  end;

  PPGTestCase = ^PGTestCase;
  PGTestCase = ^TGTestCase;

  TGTestCase = record
    Unknown: Pointer;
  end;



  PPGTestConfig = ^PGTestConfig;
  PGTestConfig = ^TGTestConfig;

  TGTestConfig = record
    test_initialized: gboolean;
    test_quick: gboolean;
    test_perf: gboolean;
    test_verbose: gboolean;
    test_quiet: gboolean;
  end;


  TGTestDataFunc = procedure(user_data: gpointer); cdecl;
  TGTestFixtureFunc = procedure(fixture: gpointer; user_data: gpointer); cdecl;
  TGTestFunc = procedure; cdecl;

  PPGTestLogMsg = ^PGTestLogMsg;
  PGTestLogMsg = ^TGTestLogMsg;

  PPGTestLogType = ^PGTestLogType;
  PGTestLogType = ^TGTestLogType;
  TGTestLogMsg = object
    log_type: TGTestLogType;
    n_strings: guint;
    strings: PPgchar;
    n_nums: guint;
    nums: Pglong;
    procedure free; cdecl; inline;
  end;

  PPGTestLogBuffer = ^PGTestLogBuffer;
  PGTestLogBuffer = ^TGTestLogBuffer;
  TGTestLogBuffer = object
    data: PGString;
    msgs: PGSList;
    procedure free; cdecl; inline;
    function pop: PGTestLogMsg; cdecl; inline;
    procedure push(n_bytes: guint; bytes: Pguint8); cdecl; inline;
    function new: PGTestLogBuffer; cdecl; inline; static;
  end;
  TGTestLogFatalFunc = function(log_domain: Pgchar; log_level: TGLogLevelFlags; message: Pgchar; user_data: gpointer): gboolean; cdecl;

  PPGTestSuite = ^PGTestSuite;
  PGTestSuite = ^TGTestSuite;
  TGTestSuite = object
    procedure add(test_case: PGTestCase); cdecl; inline;
    procedure add_suite(nestedsuite: PGTestSuite); cdecl; inline;
  end;
  TGTestTrapFlags = packed object(TBitObject32)
  public
    property silence_stdout: DWord index 128 read GetBit write SetBit;
    property silence_stderr: DWord index 256 read GetBit write SetBit;
    property inherit_stdin: DWord index 512 read GetBit write SetBit;
  end;


  PPGThreadPriority = ^PGThreadPriority;
  PGThreadPriority = ^TGThreadPriority;

  PPGThread = ^PGThread;
  PGThread = ^TGThread;

  PPGThreadFunctions = ^PGThreadFunctions;
  PGThreadFunctions = ^TGThreadFunctions;
  TGThread = object
    func: TGThreadFunc;
    data: gpointer;
    joinable: gboolean;
    priority: TGThreadPriority;
    function join: gpointer; cdecl; inline;
    procedure set_priority(priority: TGThreadPriority); cdecl; inline;
    function create_full(func: TGThreadFunc; data: gpointer; stack_size: gulong; joinable: gboolean; bound: gboolean; priority: TGThreadPriority): PGThread; cdecl; inline; static;
    function error_quark: TGQuark; cdecl; inline; static;
    procedure exit(retval: gpointer); cdecl; inline; static;
    procedure foreach(thread_func: TGFunc; user_data: gpointer); cdecl; inline; static;
    function get_initialized: gboolean; cdecl; inline; static;
    procedure init(vtable: PGThreadFunctions); cdecl; inline; static;
    procedure init_with_errorcheck_mutexes(vtable: PGThreadFunctions); cdecl; inline; static;
    function self: PGThread; cdecl; inline; static;
  end;

  TGThreadFunctions = record
    mutex_new: function: PGMutex; cdecl;
    mutex_lock: procedure(mutex: PGMutex); cdecl;
    mutex_trylock: function(mutex: PGMutex): gboolean; cdecl;
    mutex_unlock: procedure(mutex: PGMutex); cdecl;
    mutex_free: procedure(mutex: PGMutex); cdecl;
    cond_new: function: PGCond; cdecl;
    cond_signal: procedure(cond: PGCond); cdecl;
    cond_broadcast: procedure(cond: PGCond); cdecl;
    cond_wait: procedure(cond: PGCond; mutex: PGMutex); cdecl;
    cond_timed_wait: function(cond: PGCond; mutex: PGMutex; end_time: PGTimeVal): gboolean; cdecl;
    cond_free: procedure(cond: PGCond); cdecl;
    private_new: function(destructor_: TGDestroyNotify): PGPrivate; cdecl;
    private_get: function(private_key: PGPrivate): gpointer; cdecl;
    private_set: procedure(private_key: PGPrivate; data: gpointer); cdecl;
    thread_create: procedure(func: TGThreadFunc; data: gpointer; stack_size: gulong; joinable: gboolean; bound: gboolean; priority: TGThreadPriority; thread: gpointer); cdecl;
    thread_yield: procedure; cdecl;
    thread_join: procedure(thread: gpointer); cdecl;
    thread_exit: procedure; cdecl;
    thread_set_priority: procedure(thread: gpointer; priority: TGThreadPriority); cdecl;
    thread_self: procedure(thread: gpointer); cdecl;
    thread_equal: function(thread1: gpointer; thread2: gpointer): gboolean; cdecl;
  end;



  PPGThreadError = ^PGThreadError;
  PGThreadError = ^TGThreadError;

  PPGThreadPool = ^PGThreadPool;
  PGThreadPool = ^TGThreadPool;
  TGThreadPool = object
    func: TGFunc;
    user_data: gpointer;
    exclusive: gboolean;
    procedure free(immediate: gboolean; wait_: gboolean); cdecl; inline;
    function get_max_threads: gint; cdecl; inline;
    function get_num_threads: guint; cdecl; inline;
    procedure push(data: gpointer); cdecl; inline;
    procedure set_max_threads(max_threads: gint); cdecl; inline;
    procedure set_sort_function(func: TGCompareDataFunc; user_data: gpointer); cdecl; inline;
    function unprocessed: guint; cdecl; inline;
    function get_max_idle_time: guint; cdecl; inline; static;
    function get_max_unused_threads: gint; cdecl; inline; static;
    function get_num_unused_threads: guint; cdecl; inline; static;
    function new(func: TGFunc; user_data: gpointer; max_threads: gint; exclusive: gboolean): PGThreadPool; cdecl; inline; static;
    procedure set_max_idle_time(interval: guint); cdecl; inline; static;
    procedure set_max_unused_threads(max_threads: gint); cdecl; inline; static;
    procedure stop_unused_threads; cdecl; inline; static;
  end;

  PPGTimer = ^PGTimer;
  PGTimer = ^TGTimer;
  TGTimer = object
    procedure continue; cdecl; inline;
    procedure destroy_; cdecl; inline;
    function elapsed(microseconds: Pgulong): gdouble; cdecl; inline;
    procedure reset; cdecl; inline;
    procedure start; cdecl; inline;
    procedure stop; cdecl; inline;
    function new: PGTimer; cdecl; inline; static;
  end;

  PPGTrashStack = ^PGTrashStack;
  PGTrashStack = ^TGTrashStack;
  TGTrashStack = object
    next: PGTrashStack;
    function height(stack_p: PPGTrashStack): guint; cdecl; inline; static;
    function peek(stack_p: PPGTrashStack): gpointer; cdecl; inline; static;
    function pop(stack_p: PPGTrashStack): gpointer; cdecl; inline; static;
    procedure push(stack_p: PPGTrashStack; data_p: gpointer); cdecl; inline; static;
  end;
  TGTraverseFunc = function(key: gpointer; value: gpointer; data: gpointer): gboolean; cdecl;

  PPGTree = ^PGTree;
  PGTree = ^TGTree;

  PPGTraverseFunc = ^PGTraverseFunc;
  PGTraverseFunc = ^TGTraverseFunc;
  TGTree = object
    procedure destroy_; cdecl; inline;
    procedure foreach(func: TGTraverseFunc; user_data: gpointer); cdecl; inline;
    function height: gint; cdecl; inline;
    procedure insert(key: gpointer; value: gpointer); cdecl; inline;
    function lookup(key: gpointer): gpointer; cdecl; inline;
    function lookup_extended(lookup_key: gpointer; orig_key: Pgpointer; value: Pgpointer): gboolean; cdecl; inline;
    function nnodes: gint; cdecl; inline;
    function ref: PGTree; cdecl; inline;
    function remove(key: gpointer): gboolean; cdecl; inline;
    procedure replace(key: gpointer; value: gpointer); cdecl; inline;
    function search(search_func: TGCompareFunc; user_data: gpointer): gpointer; cdecl; inline;
    function steal(key: gpointer): gboolean; cdecl; inline;
    procedure traverse(traverse_func: TGTraverseFunc; traverse_type: TGTraverseType; user_data: gpointer); cdecl; inline;
    procedure unref; cdecl; inline;
    function new(key_compare_func: TGCompareFunc): PGTree; cdecl; inline; static;
    function new_full(key_compare_func: TGCompareDataFunc; key_compare_data: gpointer; key_destroy_func: TGDestroyNotify; value_destroy_func: TGDestroyNotify): PGTree; cdecl; inline; static;
    function new_with_data(key_compare_func: TGCompareDataFunc; key_compare_data: gpointer): PGTree; cdecl; inline; static;
  end;

  PPGUnicodeBreakType = ^PGUnicodeBreakType;
  PGUnicodeBreakType = ^TGUnicodeBreakType;

  PPGUnicodeScript = ^PGUnicodeScript;
  PGUnicodeScript = ^TGUnicodeScript;

  PPGUnicodeType = ^PGUnicodeType;
  PGUnicodeType = ^TGUnicodeType;

  PPGUserDirectory = ^PGUserDirectory;
  PGUserDirectory = ^TGUserDirectory;

  PPGVariant = ^PGVariant;
  PGVariant = ^TGVariant;

  PPGVariantType = ^PGVariantType;
  PGVariantType = ^TGVariantType;

  PPgint16 = ^Pgint16;
  Pgint16 = ^gint16;

  PPguint16 = ^Pguint16;
  Pguint16 = ^guint16;

  PPGVariantClass = ^PGVariantClass;
  PGVariantClass = ^TGVariantClass;

  PPGVariantIter = ^PGVariantIter;
  PGVariantIter = ^TGVariantIter;
  TGVariant = object
    //function new(format_string: Pgchar; args: array of const): PGVariant; cdecl; inline; static;
    function new_array(child_type: PGVariantType; children: PPGVariant; n_children: gsize): PGVariant; cdecl; inline; static;
    function new_boolean(value: gboolean): PGVariant; cdecl; inline; static;
    function new_byte(value: guint8): PGVariant; cdecl; inline; static;
    function new_bytestring(string_: Pgchar): PGVariant; cdecl; inline; static;
    function new_bytestring_array(strv: PPgchar; length: gssize): PGVariant; cdecl; inline; static;
    function new_dict_entry(key: PGVariant; value: PGVariant): PGVariant; cdecl; inline; static;
    function new_double(value: gdouble): PGVariant; cdecl; inline; static;
    function new_from_data(type_: PGVariantType; data: guint8; size: gsize; trusted: gboolean; notify: TGDestroyNotify; user_data: gpointer): PGVariant; cdecl; inline; static;
    function new_handle(value: gint32): PGVariant; cdecl; inline; static;
    function new_int16(value: gint16): PGVariant; cdecl; inline; static;
    function new_int32(value: gint32): PGVariant; cdecl; inline; static;
    function new_int64(value: gint64): PGVariant; cdecl; inline; static;
    function new_maybe(child_type: PGVariantType; child: PGVariant): PGVariant; cdecl; inline; static;
    function new_object_path(object_path: Pgchar): PGVariant; cdecl; inline; static;
    function new_objv(strv: PPgchar; length: gssize): PGVariant; cdecl; inline; static;
    //function new_parsed(format: Pgchar; args: array of const): PGVariant; cdecl; inline; static;
    //function new_parsed_va(format: Pgchar; app: Pva_list): PGVariant; cdecl; inline; static;
    function new_signature(signature: Pgchar): PGVariant; cdecl; inline; static;
    function new_string(string_: Pgchar): PGVariant; cdecl; inline; static;
    function new_strv(strv: PPgchar; length: gssize): PGVariant; cdecl; inline; static;
    function new_tuple(children: PPGVariant; n_children: gsize): PGVariant; cdecl; inline; static;
    function new_uint16(value: guint16): PGVariant; cdecl; inline; static;
    function new_uint32(value: guint32): PGVariant; cdecl; inline; static;
    function new_uint64(value: guint64): PGVariant; cdecl; inline; static;
    //function new_va(format_string: Pgchar; endptr: PPgchar; app: Pva_list): PGVariant; cdecl; inline; static;
    function new_variant(value: PGVariant): PGVariant; cdecl; inline; static;
    function byteswap: PGVariant; cdecl; inline;
    function classify: TGVariantClass; cdecl; inline;
    function compare(two: TGVariant): gint; cdecl; inline;
    function dup_bytestring(length: Pgsize): Pgchar; cdecl; inline;
    function dup_bytestring_array(length: Pgsize): PPgchar; cdecl; inline;
    function dup_objv(length: Pgsize): PPgchar; cdecl; inline;
    function dup_string(length: Pgsize): Pgchar; cdecl; inline;
    function dup_strv(length: Pgsize): PPgchar; cdecl; inline;
    function equal(two: TGVariant): gboolean; cdecl; inline;
    //procedure get(format_string: Pgchar; args: array of const); cdecl; inline;
    function get_boolean: gboolean; cdecl; inline;
    function get_byte: guint8; cdecl; inline;
    function get_bytestring: Pgchar; cdecl; inline;
    function get_bytestring_array(length: Pgsize): PPgchar; cdecl; inline;
    //procedure get_child(index_: gsize; format_string: Pgchar; args: array of const); cdecl; inline;
    function get_child_value(index_: gsize): PGVariant; cdecl; inline;
    function get_data: gpointer; cdecl; inline;
    function get_double: gdouble; cdecl; inline;
    function get_fixed_array(n_elements: Pgsize; element_size: gsize): gpointer; cdecl; inline;
    function get_handle: gint32; cdecl; inline;
    function get_int16: gint16; cdecl; inline;
    function get_int32: gint32; cdecl; inline;
    function get_int64: gint64; cdecl; inline;
    function get_maybe: PGVariant; cdecl; inline;
    function get_normal_form: PGVariant; cdecl; inline;
    function get_objv(length: Pgsize): PPgchar; cdecl; inline;
    function get_size: gsize; cdecl; inline;
    function get_string(length: Pgsize): Pgchar; cdecl; inline;
    function get_strv(length: Pgsize): PPgchar; cdecl; inline;
    function get_type_string: Pgchar; cdecl; inline;
    function get_uint16: guint16; cdecl; inline;
    function get_uint32: guint32; cdecl; inline;
    function get_uint64: guint64; cdecl; inline;
    //procedure get_va(format_string: Pgchar; endptr: PPgchar; app: Pva_list); cdecl; inline;
    function get_variant: PGVariant; cdecl; inline;
    function hash: guint; cdecl; inline;
    function is_container: gboolean; cdecl; inline;
    function is_floating: gboolean; cdecl; inline;
    function is_normal_form: gboolean; cdecl; inline;
    function is_of_type(type_: PGVariantType): gboolean; cdecl; inline;
    function iter_new: PGVariantIter; cdecl; inline;
    //function lookup(key: Pgchar; format_string: Pgchar; args: array of const): gboolean; cdecl; inline;
    function lookup_value(key: Pgchar; expected_type: PGVariantType): PGVariant; cdecl; inline;
    function n_children: gsize; cdecl; inline;
    function print(type_annotate: gboolean): Pgchar; cdecl; inline;
    function print_string(string_: PGString; type_annotate: gboolean): PGString; cdecl; inline;
    function ref: PGVariant; cdecl; inline;
    function ref_sink: PGVariant; cdecl; inline;
    procedure store(data: gpointer); cdecl; inline;
    function take_ref: PGVariant; cdecl; inline;
    procedure unref; cdecl; inline;
    function is_object_path(string_: Pgchar): gboolean; cdecl; inline; static;
    function is_signature(string_: Pgchar): gboolean; cdecl; inline; static;
    function parse(type_: PGVariantType; text: Pgchar; limit: Pgchar; endptr: PPgchar): PGVariant; cdecl; inline; static;
    function parser_get_error_quark: TGQuark; cdecl; inline; static;
  end;
  TGVariantType = object
    function new(type_string: Pgchar): PGVariantType; cdecl; inline; static;
    function new_tuple(items: PPGVariantType; length: gint): PGVariantType; cdecl; inline; static;
    function copy: PGVariantType; cdecl; inline;
    function dup_string: Pgchar; cdecl; inline;
    function element: PGVariantType; cdecl; inline;
    function equal(type2: TGVariantType): gboolean; cdecl; inline;
    function first: PGVariantType; cdecl; inline;
    procedure free; cdecl; inline;
    function get_string_length: gsize; cdecl; inline;
    function hash: guint; cdecl; inline;
    function is_array: gboolean; cdecl; inline;
    function is_basic: gboolean; cdecl; inline;
    function is_container: gboolean; cdecl; inline;
    function is_definite: gboolean; cdecl; inline;
    function is_dict_entry: gboolean; cdecl; inline;
    function is_maybe: gboolean; cdecl; inline;
    function is_subtype_of(supertype: PGVariantType): gboolean; cdecl; inline;
    function is_tuple: gboolean; cdecl; inline;
    function is_variant: gboolean; cdecl; inline;
    function key: PGVariantType; cdecl; inline;
    function n_items: gsize; cdecl; inline;
    function new_array: PGVariantType; cdecl; inline;
    function new_dict_entry(value: PGVariantType): PGVariantType; cdecl; inline;
    function new_maybe: PGVariantType; cdecl; inline;
    function next: PGVariantType; cdecl; inline;
    function peek_string: Pgchar; cdecl; inline;
    function value: PGVariantType; cdecl; inline;
    function checked_(param0: Pgchar): PGVariantType; cdecl; inline; static;
    function string_is_valid(type_string: Pgchar): gboolean; cdecl; inline; static;
    function string_scan(string_: Pgchar; limit: Pgchar; endptr: PPgchar): gboolean; cdecl; inline; static;
  end;
  TGVariantIter = object
    x: array [0..15] of gsize;
    function copy: PGVariantIter; cdecl; inline;
    procedure free; cdecl; inline;
    function init(value: PGVariant): gsize; cdecl; inline;
    //function loop(format_string: Pgchar; args: array of const): gboolean; cdecl; inline;
    function n_children: gsize; cdecl; inline;
    //function next(format_string: Pgchar; args: array of const): gboolean; cdecl; inline;
    function next_value: PGVariant; cdecl; inline;
  end;

  PPGVariantBuilder = ^PGVariantBuilder;
  PGVariantBuilder = ^TGVariantBuilder;
  TGVariantBuilder = object
    x: array [0..15] of gsize;
    function new(type_: PGVariantType): PGVariantBuilder; cdecl; inline; static;
    //procedure add(format_string: Pgchar; args: array of const); cdecl; inline;
    //procedure add_parsed(format: Pgchar; args: array of const); cdecl; inline;
    procedure add_value(value: PGVariant); cdecl; inline;
    procedure clear; cdecl; inline;
    procedure close; cdecl; inline;
    function end_: PGVariant; cdecl; inline;
    procedure init(type_: PGVariantType); cdecl; inline;
    procedure open(type_: PGVariantType); cdecl; inline;
    function ref: PGVariantBuilder; cdecl; inline;
    procedure unref; cdecl; inline;
  end;

  PPGVariantParseError = ^PGVariantParseError;
  PGVariantParseError = ^TGVariantParseError;
  TGVoidFunc = procedure; cdecl;

  PP_GStaticAssert_347 = ^P_GStaticAssert_347;
  P_GStaticAssert_347 = ^T_GStaticAssert_347;

  T_GStaticAssert_347 = record
    Compile_Time_Assertion: array [0..0] of gchar;
  end;



function g_access(filename: Pgchar; mode: gint): gint; cdecl; external;
function g_allocator_new(name: Pgchar; n_preallocs: guint): PGAllocator; cdecl; external;
function g_array_append_vals(array_: Pgpointer; data: gpointer; len: guint): Pgpointer; cdecl; external;
function g_array_free(array_: Pgpointer; free_segment: gboolean): Pgchar; cdecl; external;
function g_array_get_element_size(array_: Pgpointer): guint; cdecl; external;
function g_array_get_type: TGType; cdecl; external;
function g_array_insert_vals(array_: Pgpointer; index_: guint; data: gpointer; len: guint): Pgpointer; cdecl; external;
function g_array_new(zero_terminated: gboolean; clear_: gboolean; element_size: guint): Pgpointer; cdecl; external;
function g_array_prepend_vals(array_: Pgpointer; data: gpointer; len: guint): Pgpointer; cdecl; external;
function g_array_ref(array_: Pgpointer): Pgpointer; cdecl; external;
function g_array_remove_index(array_: Pgpointer; index_: guint): Pgpointer; cdecl; external;
function g_array_remove_index_fast(array_: Pgpointer; index_: guint): Pgpointer; cdecl; external;
function g_array_remove_range(array_: Pgpointer; index_: guint; length: guint): Pgpointer; cdecl; external;
function g_array_set_size(array_: Pgpointer; length: guint): Pgpointer; cdecl; external;
function g_array_sized_new(zero_terminated: gboolean; clear_: gboolean; element_size: guint; reserved_size: guint): Pgpointer; cdecl; external;
function g_ascii_digit_value(c: gchar): gint; cdecl; external;
function g_ascii_dtostr(buffer: Pgchar; buf_len: gint; d: gdouble): Pgchar; cdecl; external;
function g_ascii_formatd(buffer: Pgchar; buf_len: gint; format: Pgchar; d: gdouble): Pgchar; cdecl; external;
function g_ascii_strcasecmp(s1: Pgchar; s2: Pgchar): gint; cdecl; external;
function g_ascii_strdown(str: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_ascii_strncasecmp(s1: Pgchar; s2: Pgchar; n: gsize): gint; cdecl; external;
function g_ascii_strtod(nptr: Pgchar; endptr: PPgchar): gdouble; cdecl; external;
function g_ascii_strtoll(nptr: Pgchar; endptr: PPgchar; base: guint): gint64; cdecl; external;
function g_ascii_strtoull(nptr: Pgchar; endptr: PPgchar; base: guint): guint64; cdecl; external;
function g_ascii_strup(str: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_ascii_tolower(c: gchar): gchar; cdecl; external;
function g_ascii_toupper(c: gchar): gchar; cdecl; external;
function g_ascii_xdigit_value(c: gchar): gint; cdecl; external;
function g_async_queue_length(AAsyncQueue: PGAsyncQueue): gint; cdecl; external;
function g_async_queue_length_unlocked(AAsyncQueue: PGAsyncQueue): gint; cdecl; external;
function g_async_queue_new: PGAsyncQueue; cdecl; external;
function g_async_queue_new_full(item_free_func: TGDestroyNotify): PGAsyncQueue; cdecl; external;
function g_async_queue_pop(AAsyncQueue: PGAsyncQueue): gpointer; cdecl; external;
function g_async_queue_pop_unlocked(AAsyncQueue: PGAsyncQueue): gpointer; cdecl; external;
function g_async_queue_ref(AAsyncQueue: PGAsyncQueue): PGAsyncQueue; cdecl; external;
function g_async_queue_timed_pop(AAsyncQueue: PGAsyncQueue; end_time: PGTimeVal): gpointer; cdecl; external;
function g_async_queue_timed_pop_unlocked(AAsyncQueue: PGAsyncQueue; end_time: PGTimeVal): gpointer; cdecl; external;
function g_async_queue_try_pop(AAsyncQueue: PGAsyncQueue): gpointer; cdecl; external;
function g_async_queue_try_pop_unlocked(AAsyncQueue: PGAsyncQueue): gpointer; cdecl; external;
function g_atomic_int_add(atomic: Pgint; val: gint): gint; cdecl; external;
function g_atomic_int_and(atomic: Pguint; val: guint): guint; cdecl; external;
function g_atomic_int_compare_and_exchange(atomic: Pgint; oldval: gint; newval: gint): gboolean; cdecl; external;
function g_atomic_int_dec_and_test(atomic: Pgint): gboolean; cdecl; external;
function g_atomic_int_exchange_and_add(atomic: Pgint; val: gint): gint; cdecl; external;
function g_atomic_int_get(atomic: Pgint): gint; cdecl; external;
function g_atomic_int_or(atomic: Pguint; val: guint): guint; cdecl; external;
function g_atomic_int_xor(atomic: Pguint; val: guint): guint; cdecl; external;
function g_atomic_pointer_add(atomic: Pgpointer; val: gssize): gssize; cdecl; external;
function g_atomic_pointer_and(atomic: Pgpointer; val: gsize): gsize; cdecl; external;
function g_atomic_pointer_compare_and_exchange(atomic: Pgpointer; oldval: gpointer; newval: gpointer): gboolean; cdecl; external;
function g_atomic_pointer_get(atomic: Pgpointer): gpointer; cdecl; external;
function g_atomic_pointer_or(atomic: Pgpointer; val: gsize): gsize; cdecl; external;
function g_atomic_pointer_xor(atomic: Pgpointer; val: gsize): gsize; cdecl; external;
function g_base64_decode(text: Pgchar; out_len: Pgsize): Pguint8; cdecl; external;
function g_base64_decode_inplace(text: Pgchar; out_len: Pgsize): Pguint8; cdecl; external;
function g_base64_decode_step(in_: Pgchar; len: gsize; out_: Pguint8; state: Pgint; save: Pguint): gsize; cdecl; external;
function g_base64_encode(data: Pguint8; len: gsize): Pgchar; cdecl; external;
function g_base64_encode_close(break_lines: gboolean; out_: Pgchar; state: Pgint; save: Pgint): gsize; cdecl; external;
function g_base64_encode_step(in_: Pguint8; len: gsize; break_lines: gboolean; out_: Pgchar; state: Pgint; save: Pgint): gsize; cdecl; external;
function g_basename(file_name: Pgchar): Pgchar; cdecl; external;
function g_bit_nth_lsf(mask: gulong; nth_bit: gint): gint; cdecl; external;
function g_bit_nth_msf(mask: gulong; nth_bit: gint): gint; cdecl; external;
function g_bit_storage(number: gulong): guint; cdecl; external;
function g_bit_trylock(address: Pgint; lock_bit: gint): gboolean; cdecl; external;
function g_bookmark_file_error_quark: TGQuark; cdecl; external;
function g_bookmark_file_get_added(ABookmarkFile: PGBookmarkFile; uri: Pgchar): glong; cdecl; external;
function g_bookmark_file_get_app_info(ABookmarkFile: PGBookmarkFile; uri: Pgchar; name: Pgchar; exec: PPgchar; count: Pguint; stamp: Pglong): gboolean; cdecl; external;
function g_bookmark_file_get_applications(ABookmarkFile: PGBookmarkFile; uri: Pgchar; length: Pgsize): PPgchar; cdecl; external;
function g_bookmark_file_get_description(ABookmarkFile: PGBookmarkFile; uri: Pgchar): Pgchar; cdecl; external;
function g_bookmark_file_get_groups(ABookmarkFile: PGBookmarkFile; uri: Pgchar; length: Pgsize): PPgchar; cdecl; external;
function g_bookmark_file_get_icon(ABookmarkFile: PGBookmarkFile; uri: Pgchar; href: PPgchar; mime_type: PPgchar): gboolean; cdecl; external;
function g_bookmark_file_get_is_private(ABookmarkFile: PGBookmarkFile; uri: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_get_mime_type(ABookmarkFile: PGBookmarkFile; uri: Pgchar): Pgchar; cdecl; external;
function g_bookmark_file_get_modified(ABookmarkFile: PGBookmarkFile; uri: Pgchar): glong; cdecl; external;
function g_bookmark_file_get_size(ABookmarkFile: PGBookmarkFile): gint; cdecl; external;
function g_bookmark_file_get_title(ABookmarkFile: PGBookmarkFile; uri: Pgchar): Pgchar; cdecl; external;
function g_bookmark_file_get_uris(ABookmarkFile: PGBookmarkFile; length: Pgsize): PPgchar; cdecl; external;
function g_bookmark_file_get_visited(ABookmarkFile: PGBookmarkFile; uri: Pgchar): glong; cdecl; external;
function g_bookmark_file_has_application(ABookmarkFile: PGBookmarkFile; uri: Pgchar; name: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_has_group(ABookmarkFile: PGBookmarkFile; uri: Pgchar; group: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_has_item(ABookmarkFile: PGBookmarkFile; uri: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_load_from_data(ABookmarkFile: PGBookmarkFile; data: Pgchar; length: gsize): gboolean; cdecl; external;
function g_bookmark_file_load_from_data_dirs(ABookmarkFile: PGBookmarkFile; file_: Pgchar; full_path: PPgchar): gboolean; cdecl; external;
function g_bookmark_file_load_from_file(ABookmarkFile: PGBookmarkFile; filename: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_move_item(ABookmarkFile: PGBookmarkFile; old_uri: Pgchar; new_uri: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_new: PGBookmarkFile; cdecl; external;
function g_bookmark_file_remove_application(ABookmarkFile: PGBookmarkFile; uri: Pgchar; name: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_remove_group(ABookmarkFile: PGBookmarkFile; uri: Pgchar; group: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_remove_item(ABookmarkFile: PGBookmarkFile; uri: Pgchar): gboolean; cdecl; external;
function g_bookmark_file_set_app_info(ABookmarkFile: PGBookmarkFile; uri: Pgchar; name: Pgchar; exec: Pgchar; count: gint; stamp: glong): gboolean; cdecl; external;
function g_bookmark_file_to_data(ABookmarkFile: PGBookmarkFile; length: Pgsize): Pgchar; cdecl; external;
function g_bookmark_file_to_file(ABookmarkFile: PGBookmarkFile; filename: Pgchar): gboolean; cdecl; external;
function g_build_filename(first_element: Pgchar; args: array of const): Pgchar; cdecl; external;
function g_build_filenamev(args: PPgchar): Pgchar; cdecl; external;
function g_build_path(separator: Pgchar; first_element: Pgchar; args: array of const): Pgchar; cdecl; external;
function g_build_pathv(separator: Pgchar; args: PPgchar): Pgchar; cdecl; external;
function g_byte_array_append(array_: Pguint8; data: Pguint8; len: guint): Pguint8; cdecl; external;
function g_byte_array_free(array_: Pguint8; free_segment: gboolean): Pguint8; cdecl; external;
function g_byte_array_get_type: TGType; cdecl; external;
function g_byte_array_new: Pguint8; cdecl; external;
function g_byte_array_prepend(array_: Pguint8; data: Pguint8; len: guint): Pguint8; cdecl; external;
function g_byte_array_ref(array_: Pguint8): Pguint8; cdecl; external;
function g_byte_array_remove_index(array_: Pguint8; index_: guint): Pguint8; cdecl; external;
function g_byte_array_remove_index_fast(array_: Pguint8; index_: guint): Pguint8; cdecl; external;
function g_byte_array_remove_range(array_: Pguint8; index_: guint; length: guint): Pguint8; cdecl; external;
function g_byte_array_set_size(array_: Pguint8; length: guint): Pguint8; cdecl; external;
function g_byte_array_sized_new(reserved_size: guint): Pguint8; cdecl; external;
function g_cache_insert(ACache: PGCache; key: gpointer): gpointer; cdecl; external;
function g_cache_new(value_new_func: TGCacheNewFunc; value_destroy_func: TGCacheDestroyFunc; key_dup_func: TGCacheDupFunc; key_destroy_func: TGCacheDestroyFunc; hash_key_func: TGHashFunc; hash_value_func: TGHashFunc; key_equal_func: TGEqualFunc): PGCache; cdecl; external;
function g_chdir(path: Pgchar): gint; cdecl; external;
function g_checksum_copy(AChecksum: PGChecksum): PGChecksum; cdecl; external;
function g_checksum_get_string(AChecksum: PGChecksum): Pgchar; cdecl; external;
function g_checksum_new(checksum_type: TGChecksumType): PGChecksum; cdecl; external;
function g_checksum_type_get_length(checksum_type: TGChecksumType): gssize; cdecl; external;
function g_child_watch_add(pid: TGPid; function_: TGChildWatchFunc; data: gpointer): guint; cdecl; external;
function g_child_watch_add_full(priority: gint; pid: TGPid; function_: TGChildWatchFunc; data: gpointer; notify: TGDestroyNotify): guint; cdecl; external;
function g_child_watch_source_new(pid: TGPid): PGSource; cdecl; external;
function g_completion_new(func: TGCompletionFunc): PGCompletion; cdecl; external;
function g_compute_checksum_for_data(checksum_type: TGChecksumType; data: Pguint8; length: gsize): Pgchar; cdecl; external;
function g_compute_checksum_for_string(checksum_type: TGChecksumType; str: Pgchar; length: gssize): Pgchar; cdecl; external;
function g_compute_hmac_for_data(digest_type: TGChecksumType; key: Pguint8; key_len: gsize; data: Pguint8; length: gsize): Pgchar; cdecl; external;
function g_compute_hmac_for_string(digest_type: TGChecksumType; key: Pguint8; key_len: gsize; str: Pgchar; length: gssize): Pgchar; cdecl; external;
function g_convert(str: Pgchar; len: gssize; to_codeset: Pgchar; from_codeset: Pgchar; bytes_read: Pgsize; bytes_written: Pgsize): Pgchar; cdecl; external;
function g_convert_error_quark: TGQuark; cdecl; external;
function g_convert_with_fallback(str: Pgchar; len: gssize; to_codeset: Pgchar; from_codeset: Pgchar; fallback: Pgchar; bytes_read: Pgsize; bytes_written: Pgsize): Pgchar; cdecl; external;
function g_convert_with_iconv(str: Pgchar; len: gssize; converter: TGIConv; bytes_read: Pgsize; bytes_written: Pgsize): Pgchar; cdecl; external;
function g_datalist_get_data(datalist: PPGData; key: Pgchar): gpointer; cdecl; external;
function g_datalist_get_flags(datalist: PPGData): guint; cdecl; external;
function g_datalist_id_get_data(datalist: PPGData; key_id: TGQuark): gpointer; cdecl; external;
function g_datalist_id_remove_no_notify(datalist: PPGData; key_id: TGQuark): gpointer; cdecl; external;
function g_dataset_id_get_data(dataset_location: gpointer; key_id: TGQuark): gpointer; cdecl; external;
function g_dataset_id_remove_no_notify(dataset_location: gpointer; key_id: TGQuark): gpointer; cdecl; external;
function g_date_compare(ADate: PGDate; rhs: PGDate): gint; cdecl; external;
function g_date_days_between(ADate: PGDate; date2: PGDate): gint; cdecl; external;
function g_date_get_day(ADate: PGDate): TGDateDay; cdecl; external;
function g_date_get_day_of_year(ADate: PGDate): guint; cdecl; external;
function g_date_get_days_in_month(month: TGDateMonth; year: TGDateYear): guint8; cdecl; external;
function g_date_get_iso8601_week_of_year(ADate: PGDate): guint; cdecl; external;
function g_date_get_julian(ADate: PGDate): guint32; cdecl; external;
function g_date_get_monday_week_of_year(ADate: PGDate): guint; cdecl; external;
function g_date_get_monday_weeks_in_year(year: TGDateYear): guint8; cdecl; external;
function g_date_get_month(ADate: PGDate): TGDateMonth; cdecl; external;
function g_date_get_sunday_week_of_year(ADate: PGDate): guint; cdecl; external;
function g_date_get_sunday_weeks_in_year(year: TGDateYear): guint8; cdecl; external;
function g_date_get_type: TGType; cdecl; external;
function g_date_get_weekday(ADate: PGDate): TGDateWeekday; cdecl; external;
function g_date_get_year(ADate: PGDate): TGDateYear; cdecl; external;
function g_date_is_first_of_month(ADate: PGDate): gboolean; cdecl; external;
function g_date_is_last_of_month(ADate: PGDate): gboolean; cdecl; external;
function g_date_is_leap_year(year: TGDateYear): gboolean; cdecl; external;
function g_date_new: PGDate; cdecl; external;
function g_date_new_dmy(day: TGDateDay; month: TGDateMonth; year: TGDateYear): PGDate; cdecl; external;
function g_date_new_julian(julian_day: guint32): PGDate; cdecl; external;
function g_date_strftime(s: Pgchar; slen: gsize; format: Pgchar; date: PGDate): gsize; cdecl; external;
function g_date_time_add(ADateTime: PGDateTime; timespan: TGTimeSpan): PGDateTime; cdecl; external;
function g_date_time_add_days(ADateTime: PGDateTime; days: gint): PGDateTime; cdecl; external;
function g_date_time_add_full(ADateTime: PGDateTime; years: gint; months: gint; days: gint; hours: gint; minutes: gint; seconds: gdouble): PGDateTime; cdecl; external;
function g_date_time_add_hours(ADateTime: PGDateTime; hours: gint): PGDateTime; cdecl; external;
function g_date_time_add_minutes(ADateTime: PGDateTime; minutes: gint): PGDateTime; cdecl; external;
function g_date_time_add_months(ADateTime: PGDateTime; months: gint): PGDateTime; cdecl; external;
function g_date_time_add_seconds(ADateTime: PGDateTime; seconds: gdouble): PGDateTime; cdecl; external;
function g_date_time_add_weeks(ADateTime: PGDateTime; weeks: gint): PGDateTime; cdecl; external;
function g_date_time_add_years(ADateTime: PGDateTime; years: gint): PGDateTime; cdecl; external;
function g_date_time_compare(dt1: gpointer; dt2: gpointer): gint; cdecl; external;
function g_date_time_difference(ADateTime: PGDateTime; begin_: PGDateTime): TGTimeSpan; cdecl; external;
function g_date_time_equal(dt1: gpointer; dt2: gpointer): gboolean; cdecl; external;
function g_date_time_format(ADateTime: PGDateTime; format: Pgchar): Pgchar; cdecl; external;
function g_date_time_get_day_of_month(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_day_of_week(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_day_of_year(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_hour(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_microsecond(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_minute(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_month(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_second(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_seconds(ADateTime: PGDateTime): gdouble; cdecl; external;
function g_date_time_get_timezone_abbreviation(ADateTime: PGDateTime): Pgchar; cdecl; external;
function g_date_time_get_type: TGType; cdecl; external;
function g_date_time_get_utc_offset(ADateTime: PGDateTime): TGTimeSpan; cdecl; external;
function g_date_time_get_week_numbering_year(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_week_of_year(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_get_year(ADateTime: PGDateTime): gint; cdecl; external;
function g_date_time_hash(datetime: gpointer): guint; cdecl; external;
function g_date_time_is_daylight_savings(ADateTime: PGDateTime): gboolean; cdecl; external;
function g_date_time_new(tz: PGTimeZone; year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl; external;
function g_date_time_new_from_timeval_local(tv: PGTimeVal): PGDateTime; cdecl; external;
function g_date_time_new_from_timeval_utc(tv: PGTimeVal): PGDateTime; cdecl; external;
function g_date_time_new_from_unix_local(t: gint64): PGDateTime; cdecl; external;
function g_date_time_new_from_unix_utc(t: gint64): PGDateTime; cdecl; external;
function g_date_time_new_local(year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl; external;
function g_date_time_new_now(tz: PGTimeZone): PGDateTime; cdecl; external;
function g_date_time_new_now_local: PGDateTime; cdecl; external;
function g_date_time_new_now_utc: PGDateTime; cdecl; external;
function g_date_time_new_utc(year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl; external;
function g_date_time_ref(ADateTime: PGDateTime): PGDateTime; cdecl; external;
function g_date_time_source_new(ADateTime: PGDateTime; cancel_on_set: gboolean): PGSource; cdecl; external;
function g_date_time_to_local(ADateTime: PGDateTime): PGDateTime; cdecl; external;
function g_date_time_to_timeval(ADateTime: PGDateTime; tv: PGTimeVal): gboolean; cdecl; external;
function g_date_time_to_timezone(ADateTime: PGDateTime; tz: PGTimeZone): PGDateTime; cdecl; external;
function g_date_time_to_unix(ADateTime: PGDateTime): gint64; cdecl; external;
function g_date_time_to_utc(ADateTime: PGDateTime): PGDateTime; cdecl; external;
function g_date_valid(ADate: PGDate): gboolean; cdecl; external;
function g_date_valid_day(day: TGDateDay): gboolean; cdecl; external;
function g_date_valid_dmy(day: TGDateDay; month: TGDateMonth; year: TGDateYear): gboolean; cdecl; external;
function g_date_valid_julian(julian_date: guint32): gboolean; cdecl; external;
function g_date_valid_month(month: TGDateMonth): gboolean; cdecl; external;
function g_date_valid_weekday(weekday: TGDateWeekday): gboolean; cdecl; external;
function g_date_valid_year(year: TGDateYear): gboolean; cdecl; external;
function g_dcgettext(domain: Pgchar; msgid: Pgchar; category: gint): Pgchar; cdecl; external;
function g_dgettext(domain: Pgchar; msgid: Pgchar): Pgchar; cdecl; external;
function g_dir_make_tmp(tmpl: Pgchar): Pgchar; cdecl; external;
function g_dir_open(path: Pgchar; flags: guint): PGDir; cdecl; external;
function g_dir_read_name(ADir: PGDir): Pgchar; cdecl; external;
function g_direct_equal(v1: gpointer; v2: gpointer): gboolean; cdecl; external;
function g_direct_hash(v: gpointer): guint; cdecl; external;
function g_dngettext(domain: Pgchar; msgid: Pgchar; msgid_plural: Pgchar; n: gulong): Pgchar; cdecl; external;
function g_double_equal(v1: gpointer; v2: gpointer): gboolean; cdecl; external;
function g_double_hash(v: gpointer): guint; cdecl; external;
function g_dpgettext(domain: Pgchar; msgctxtid: Pgchar; msgidoffset: gsize): Pgchar; cdecl; external;
function g_dpgettext2(domain: Pgchar; context: Pgchar; msgid: Pgchar): Pgchar; cdecl; external;
function g_error_copy(AError: PGError): PGError; cdecl; external;
function g_error_get_type: TGType; cdecl; external;
function g_error_matches(AError: PGError; domain: TGQuark; code: gint): gboolean; cdecl; external;
function g_error_new(domain: TGQuark; code: gint; format: Pgchar; args: array of const): PGError; cdecl; external;
function g_error_new_literal(domain: TGQuark; code: gint; message: Pgchar): PGError; cdecl; external;
function g_error_new_valist(domain: TGQuark; code: gint; format: Pgchar; args: Tva_list): PGError; cdecl; external;
function g_file_error_from_errno(err_no: gint): TGFileError; cdecl; external;
function g_file_error_quark: TGQuark; cdecl; external;
function g_file_get_contents(filename: Pgchar; contents: PPgchar; length: Pgsize): gboolean; cdecl; external;
function g_file_open_tmp(tmpl: Pgchar; name_used: PPgchar): gint; cdecl; external;
function g_file_read_link(filename: Pgchar): Pgchar; cdecl; external;
function g_file_set_contents(filename: Pgchar; contents: Pgchar; length: gssize): gboolean; cdecl; external;
function g_file_test(filename: Pgchar; test: TGFileTest): gboolean; cdecl; external;
function g_filename_display_basename(filename: Pgchar): Pgchar; cdecl; external;
function g_filename_display_name(filename: Pgchar): Pgchar; cdecl; external;
function g_filename_from_uri(uri: Pgchar; hostname: PPgchar): Pgchar; cdecl; external;
function g_filename_from_utf8(utf8string: Pgchar; len: gssize; bytes_read: Pgsize; bytes_written: Pgsize): Pgchar; cdecl; external;
function g_filename_to_uri(filename: Pgchar; hostname: Pgchar): Pgchar; cdecl; external;
function g_filename_to_utf8(opsysstring: Pgchar; len: gssize; bytes_read: Pgsize; bytes_written: Pgsize): Pgchar; cdecl; external;
function g_find_program_in_path(program_: Pgchar): Pgchar; cdecl; external;
function g_format_size(size: guint64): Pgchar; cdecl; external;
function g_format_size_for_display(size: gint64): Pgchar; cdecl; external;
function g_format_size_full(size: guint64; flags: TGFormatSizeFlags): Pgchar; cdecl; external;
function g_fprintf(file_: Pgpointer; format: Pgchar; args: array of const): gint; cdecl; external;
function g_get_application_name: Pgchar; cdecl; external;
function g_get_charset(charset: PPgchar): gboolean; cdecl; external;
function g_get_current_dir: Pgchar; cdecl; external;
function g_get_environ: PPgchar; cdecl; external;
function g_get_filename_charsets(charsets: PPPgchar): gboolean; cdecl; external;
function g_get_home_dir: Pgchar; cdecl; external;
function g_get_host_name: Pgchar; cdecl; external;
function g_get_language_names: PPgchar; cdecl; external;
function g_get_locale_variants(locale: Pgchar): PPgchar; cdecl; external;
function g_get_monotonic_time: gint64; cdecl; external;
function g_get_prgname: Pgchar; cdecl; external;
function g_get_real_name: Pgchar; cdecl; external;
function g_get_real_time: gint64; cdecl; external;
function g_get_system_config_dirs: PPgchar; cdecl; external;
function g_get_system_data_dirs: PPgchar; cdecl; external;
function g_get_tmp_dir: Pgchar; cdecl; external;
function g_get_user_cache_dir: Pgchar; cdecl; external;
function g_get_user_config_dir: Pgchar; cdecl; external;
function g_get_user_data_dir: Pgchar; cdecl; external;
function g_get_user_name: Pgchar; cdecl; external;
function g_get_user_runtime_dir: Pgchar; cdecl; external;
function g_get_user_special_dir(directory: TGUserDirectory): Pgchar; cdecl; external;
function g_getenv(variable: Pgchar): Pgchar; cdecl; external;
function g_gstring_get_type: TGType; cdecl; external;
function g_hash_table_find(hash_table: PGHashTable; predicate: TGHRFunc; user_data: gpointer): gpointer; cdecl; external;
function g_hash_table_foreach_remove(hash_table: PGHashTable; func: TGHRFunc; user_data: gpointer): guint; cdecl; external;
function g_hash_table_foreach_steal(hash_table: PGHashTable; func: TGHRFunc; user_data: gpointer): guint; cdecl; external;
function g_hash_table_get_keys(hash_table: PGHashTable): PGList; cdecl; external;
function g_hash_table_get_type: TGType; cdecl; external;
function g_hash_table_get_values(hash_table: PGHashTable): PGList; cdecl; external;
function g_hash_table_iter_get_hash_table(AHashTableIter: PGHashTableIter): PGHashTable; cdecl; external;
function g_hash_table_iter_next(AHashTableIter: PGHashTableIter; key: Pgpointer; value: Pgpointer): gboolean; cdecl; external;
function g_hash_table_lookup(hash_table: PGHashTable; key: gpointer): gpointer; cdecl; external;
function g_hash_table_lookup_extended(hash_table: PGHashTable; lookup_key: gpointer; orig_key: Pgpointer; value: Pgpointer): gboolean; cdecl; external;
function g_hash_table_new(hash_func: TGHashFunc; key_equal_func: TGEqualFunc): PGHashTable; cdecl; external;
function g_hash_table_new_full(hash_func: TGHashFunc; key_equal_func: TGEqualFunc; key_destroy_func: TGDestroyNotify; value_destroy_func: TGDestroyNotify): PGHashTable; cdecl; external;
function g_hash_table_ref(hash_table: PGHashTable): PGHashTable; cdecl; external;
function g_hash_table_remove(hash_table: PGHashTable; key: gpointer): gboolean; cdecl; external;
function g_hash_table_size(hash_table: PGHashTable): guint; cdecl; external;
function g_hash_table_steal(hash_table: PGHashTable; key: gpointer): gboolean; cdecl; external;
function g_hmac_copy(AHmac: PGHmac): PGHmac; cdecl; external;
function g_hmac_get_string(AHmac: PGHmac): Pgchar; cdecl; external;
function g_hmac_new(digest_type: TGChecksumType; key: Pguint8; key_len: gsize): PGHmac; cdecl; external;
function g_hmac_ref(AHmac: PGHmac): PGHmac; cdecl; external;
function g_hook_alloc(hook_list: PGHookList): PGHook; cdecl; external;
function g_hook_compare_ids(AHook: PGHook; sibling: PGHook): gint; cdecl; external;
function g_hook_destroy(hook_list: PGHookList; hook_id: gulong): gboolean; cdecl; external;
function g_hook_find(hook_list: PGHookList; need_valids: gboolean; func: TGHookFindFunc; data: gpointer): PGHook; cdecl; external;
function g_hook_find_data(hook_list: PGHookList; need_valids: gboolean; data: gpointer): PGHook; cdecl; external;
function g_hook_find_func(hook_list: PGHookList; need_valids: gboolean; func: gpointer): PGHook; cdecl; external;
function g_hook_find_func_data(hook_list: PGHookList; need_valids: gboolean; func: gpointer; data: gpointer): PGHook; cdecl; external;
function g_hook_first_valid(hook_list: PGHookList; may_be_in_call: gboolean): PGHook; cdecl; external;
function g_hook_get(hook_list: PGHookList; hook_id: gulong): PGHook; cdecl; external;
function g_hook_next_valid(hook_list: PGHookList; hook: PGHook; may_be_in_call: gboolean): PGHook; cdecl; external;
function g_hook_ref(hook_list: PGHookList; hook: PGHook): PGHook; cdecl; external;
function g_hostname_is_ascii_encoded(hostname: Pgchar): gboolean; cdecl; external;
function g_hostname_is_ip_address(hostname: Pgchar): gboolean; cdecl; external;
function g_hostname_is_non_ascii(hostname: Pgchar): gboolean; cdecl; external;
function g_hostname_to_ascii(hostname: Pgchar): Pgchar; cdecl; external;
function g_hostname_to_unicode(hostname: Pgchar): Pgchar; cdecl; external;
function g_iconv(AIConv: PGIConv; inbuf: PPgchar; inbytes_left: Pgsize; outbuf: PPgchar; outbytes_left: Pgsize): gsize; cdecl; external;
function g_iconv_close(AIConv: PGIConv): gint; cdecl; external;
function g_iconv_open(to_codeset: Pgchar; from_codeset: Pgchar): TGIConv; cdecl; external;
function g_idle_add(function_: TGSourceFunc; data: gpointer): guint; cdecl; external;
function g_idle_add_full(priority: gint; function_: TGSourceFunc; data: gpointer; notify: TGDestroyNotify): guint; cdecl; external;
function g_idle_remove_by_data(data: gpointer): gboolean; cdecl; external;
function g_idle_source_new: PGSource; cdecl; external;
function g_int64_equal(v1: gpointer; v2: gpointer): gboolean; cdecl; external;
function g_int64_hash(v: gpointer): guint; cdecl; external;
function g_int_equal(v1: gpointer; v2: gpointer): gboolean; cdecl; external;
function g_int_hash(v: gpointer): guint; cdecl; external;
function g_intern_static_string(string_: Pgchar): Pgchar; cdecl; external;
function g_intern_string(string_: Pgchar): Pgchar; cdecl; external;
function g_io_add_watch(channel: PGIOChannel; condition: TGIOCondition; func: TGIOFunc; user_data: gpointer): guint; cdecl; external;
function g_io_add_watch_full(channel: PGIOChannel; priority: gint; condition: TGIOCondition; func: TGIOFunc; user_data: gpointer; notify: TGDestroyNotify): guint; cdecl; external;
function g_io_channel_error_from_errno(en: gint): TGIOChannelError; cdecl; external;
function g_io_channel_error_quark: TGQuark; cdecl; external;
function g_io_channel_flush(AIOChannel: PGIOChannel): TGIOStatus; cdecl; external;
function g_io_channel_get_buffer_condition(AIOChannel: PGIOChannel): TGIOCondition; cdecl; external;
function g_io_channel_get_buffer_size(AIOChannel: PGIOChannel): gsize; cdecl; external;
function g_io_channel_get_buffered(AIOChannel: PGIOChannel): gboolean; cdecl; external;
function g_io_channel_get_close_on_unref(AIOChannel: PGIOChannel): gboolean; cdecl; external;
function g_io_channel_get_encoding(AIOChannel: PGIOChannel): Pgchar; cdecl; external;
function g_io_channel_get_flags(AIOChannel: PGIOChannel): TGIOFlags; cdecl; external;
function g_io_channel_get_line_term(AIOChannel: PGIOChannel; length: Pgint): Pgchar; cdecl; external;
function g_io_channel_get_type: TGType; cdecl; external;
function g_io_channel_new_file(filename: Pgchar; mode: Pgchar): PGIOChannel; cdecl; external;
function g_io_channel_read(AIOChannel: PGIOChannel; buf: Pgchar; count: gsize; bytes_read: Pgsize): TGIOError; cdecl; external;
function g_io_channel_read_chars(AIOChannel: PGIOChannel; buf: Pgchar; count: gsize; bytes_read: Pgsize): TGIOStatus; cdecl; external;
function g_io_channel_read_line(AIOChannel: PGIOChannel; str_return: PPgchar; length: Pgsize; terminator_pos: Pgsize): TGIOStatus; cdecl; external;
function g_io_channel_read_line_string(AIOChannel: PGIOChannel; buffer: PGString; terminator_pos: Pgsize): TGIOStatus; cdecl; external;
function g_io_channel_read_to_end(AIOChannel: PGIOChannel; str_return: PPgchar; length: Pgsize): TGIOStatus; cdecl; external;
function g_io_channel_read_unichar(AIOChannel: PGIOChannel; thechar: Pgunichar): TGIOStatus; cdecl; external;
function g_io_channel_ref(AIOChannel: PGIOChannel): PGIOChannel; cdecl; external;
function g_io_channel_seek(AIOChannel: PGIOChannel; offset: gint64; type_: TGSeekType): TGIOError; cdecl; external;
function g_io_channel_seek_position(AIOChannel: PGIOChannel; offset: gint64; type_: TGSeekType): TGIOStatus; cdecl; external;
function g_io_channel_set_encoding(AIOChannel: PGIOChannel; encoding: Pgchar): TGIOStatus; cdecl; external;
function g_io_channel_set_flags(AIOChannel: PGIOChannel; flags: TGIOFlags): TGIOStatus; cdecl; external;
function g_io_channel_shutdown(AIOChannel: PGIOChannel; flush: gboolean): TGIOStatus; cdecl; external;
function g_io_channel_unix_get_fd(AIOChannel: PGIOChannel): gint; cdecl; external;
function g_io_channel_unix_new(fd: gint): PGIOChannel; cdecl; external;
function g_io_channel_write(AIOChannel: PGIOChannel; buf: Pgchar; count: gsize; bytes_written: Pgsize): TGIOError; cdecl; external;
function g_io_channel_write_chars(AIOChannel: PGIOChannel; buf: Pgchar; count: gssize; bytes_written: Pgsize): TGIOStatus; cdecl; external;
function g_io_channel_write_unichar(AIOChannel: PGIOChannel; thechar: gunichar): TGIOStatus; cdecl; external;
function g_io_create_watch(channel: PGIOChannel; condition: TGIOCondition): PGSource; cdecl; external;
function g_key_file_error_quark: TGQuark; cdecl; external;
function g_key_file_get_boolean(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): gboolean; cdecl; external;
function g_key_file_get_boolean_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; length: Pgsize): Pgboolean; cdecl; external;
function g_key_file_get_comment(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): Pgchar; cdecl; external;
function g_key_file_get_double(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): gdouble; cdecl; external;
function g_key_file_get_double_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; length: Pgsize): Pgdouble; cdecl; external;
function g_key_file_get_groups(AKeyFile: PGKeyFile; length: Pgsize): PPgchar; cdecl; external;
function g_key_file_get_int64(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): gint64; cdecl; external;
function g_key_file_get_integer(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): gint; cdecl; external;
function g_key_file_get_integer_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; length: Pgsize): Pgint; cdecl; external;
function g_key_file_get_keys(AKeyFile: PGKeyFile; group_name: Pgchar; length: Pgsize): PPgchar; cdecl; external;
function g_key_file_get_locale_string(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; locale: Pgchar): Pgchar; cdecl; external;
function g_key_file_get_locale_string_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; locale: Pgchar; length: Pgsize): PPgchar; cdecl; external;
function g_key_file_get_start_group(AKeyFile: PGKeyFile): Pgchar; cdecl; external;
function g_key_file_get_string(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): Pgchar; cdecl; external;
function g_key_file_get_string_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; length: Pgsize): PPgchar; cdecl; external;
function g_key_file_get_uint64(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): guint64; cdecl; external;
function g_key_file_get_value(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): Pgchar; cdecl; external;
function g_key_file_has_group(AKeyFile: PGKeyFile; group_name: Pgchar): gboolean; cdecl; external;
function g_key_file_has_key(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): gboolean; cdecl; external;
function g_key_file_load_from_data(AKeyFile: PGKeyFile; data: Pgchar; length: gsize; flags: TGKeyFileFlags): gboolean; cdecl; external;
function g_key_file_load_from_data_dirs(AKeyFile: PGKeyFile; file_: Pgchar; full_path: PPgchar; flags: TGKeyFileFlags): gboolean; cdecl; external;
function g_key_file_load_from_dirs(AKeyFile: PGKeyFile; file_: Pgchar; search_dirs: PPgchar; full_path: PPgchar; flags: TGKeyFileFlags): gboolean; cdecl; external;
function g_key_file_load_from_file(AKeyFile: PGKeyFile; file_: Pgchar; flags: TGKeyFileFlags): gboolean; cdecl; external;
function g_key_file_new: PGKeyFile; cdecl; external;
function g_key_file_remove_comment(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): gboolean; cdecl; external;
function g_key_file_remove_group(AKeyFile: PGKeyFile; group_name: Pgchar): gboolean; cdecl; external;
function g_key_file_remove_key(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar): gboolean; cdecl; external;
function g_key_file_set_comment(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; comment: Pgchar): gboolean; cdecl; external;
function g_key_file_to_data(AKeyFile: PGKeyFile; length: Pgsize): Pgchar; cdecl; external;
function g_list_alloc: PGList; cdecl; external;
function g_list_append(list: PGList; data: gpointer): PGList; cdecl; external;
function g_list_concat(list1: PGList; list2: PGList): PGList; cdecl; external;
function g_list_copy(list: PGList): PGList; cdecl; external;
function g_list_delete_link(list: PGList; link_: PGList): PGList; cdecl; external;
function g_list_find(list: PGList; data: gpointer): PGList; cdecl; external;
function g_list_find_custom(list: PGList; data: gpointer; func: TGCompareFunc): PGList; cdecl; external;
function g_list_first(list: PGList): PGList; cdecl; external;
function g_list_index(list: PGList; data: gpointer): gint; cdecl; external;
function g_list_insert(list: PGList; data: gpointer; position: gint): PGList; cdecl; external;
function g_list_insert_before(list: PGList; sibling: PGList; data: gpointer): PGList; cdecl; external;
function g_list_insert_sorted(list: PGList; data: gpointer; func: TGCompareFunc): PGList; cdecl; external;
function g_list_insert_sorted_with_data(list: PGList; data: gpointer; func: TGCompareDataFunc; user_data: gpointer): PGList; cdecl; external;
function g_list_last(list: PGList): PGList; cdecl; external;
function g_list_length(list: PGList): guint; cdecl; external;
function g_list_nth(list: PGList; n: guint): PGList; cdecl; external;
function g_list_nth_data(list: PGList; n: guint): gpointer; cdecl; external;
function g_list_nth_prev(list: PGList; n: guint): PGList; cdecl; external;
function g_list_position(list: PGList; llink: PGList): gint; cdecl; external;
function g_list_prepend(list: PGList; data: gpointer): PGList; cdecl; external;
function g_list_remove(list: PGList; data: gpointer): PGList; cdecl; external;
function g_list_remove_all(list: PGList; data: gpointer): PGList; cdecl; external;
function g_list_remove_link(list: PGList; llink: PGList): PGList; cdecl; external;
function g_list_reverse(list: PGList): PGList; cdecl; external;
function g_list_sort(list: PGList; compare_func: TGCompareFunc): PGList; cdecl; external;
function g_list_sort_with_data(list: PGList; compare_func: TGCompareDataFunc; user_data: gpointer): PGList; cdecl; external;
function g_listenv: PPgchar; cdecl; external;
function g_locale_from_utf8(utf8string: Pgchar; len: gssize; bytes_read: Pgsize; bytes_written: Pgsize): Pgchar; cdecl; external;
function g_locale_to_utf8(opsysstring: Pgchar; len: gssize; bytes_read: Pgsize; bytes_written: Pgsize): Pgchar; cdecl; external;
function g_log_set_always_fatal(fatal_mask: TGLogLevelFlags): TGLogLevelFlags; cdecl; external;
function g_log_set_default_handler(log_func: TGLogFunc; user_data: gpointer): TGLogFunc; cdecl; external;
function g_log_set_fatal_mask(log_domain: Pgchar; fatal_mask: TGLogLevelFlags): TGLogLevelFlags; cdecl; external;
function g_log_set_handler(log_domain: Pgchar; log_levels: TGLogLevelFlags; log_func: TGLogFunc; user_data: gpointer): guint; cdecl; external;
function g_main_context_acquire(AMainContext: PGMainContext): gboolean; cdecl; external;
function g_main_context_check(AMainContext: PGMainContext; max_priority: gint; fds: PGPollFD; n_fds: gint): gint; cdecl; external;
function g_main_context_default: PGMainContext; cdecl; external;
function g_main_context_find_source_by_funcs_user_data(AMainContext: PGMainContext; funcs: PGSourceFuncs; user_data: gpointer): PGSource; cdecl; external;
function g_main_context_find_source_by_id(AMainContext: PGMainContext; source_id: guint): PGSource; cdecl; external;
function g_main_context_find_source_by_user_data(AMainContext: PGMainContext; user_data: gpointer): PGSource; cdecl; external;
function g_main_context_get_poll_func(AMainContext: PGMainContext): TGPollFunc; cdecl; external;
function g_main_context_get_thread_default: PGMainContext; cdecl; external;
function g_main_context_is_owner(AMainContext: PGMainContext): gboolean; cdecl; external;
function g_main_context_iteration(AMainContext: PGMainContext; may_block: gboolean): gboolean; cdecl; external;
function g_main_context_new: PGMainContext; cdecl; external;
function g_main_context_pending(AMainContext: PGMainContext): gboolean; cdecl; external;
function g_main_context_prepare(AMainContext: PGMainContext; priority: Pgint): gboolean; cdecl; external;
function g_main_context_query(AMainContext: PGMainContext; max_priority: gint; timeout_: Pgint; fds: PGPollFD; n_fds: gint): gint; cdecl; external;
function g_main_context_ref(AMainContext: PGMainContext): PGMainContext; cdecl; external;
function g_main_context_wait(AMainContext: PGMainContext; cond: PGCond; mutex: PGMutex): gboolean; cdecl; external;
function g_main_current_source: PGSource; cdecl; external;
function g_main_depth: gint; cdecl; external;
function g_main_loop_get_context(AMainLoop: PGMainLoop): PGMainContext; cdecl; external;
function g_main_loop_is_running(AMainLoop: PGMainLoop): gboolean; cdecl; external;
function g_main_loop_new(context: PGMainContext; is_running: gboolean): PGMainLoop; cdecl; external;
function g_main_loop_ref(AMainLoop: PGMainLoop): PGMainLoop; cdecl; external;
function g_malloc(n_bytes: gsize): gpointer; cdecl; external;
function g_malloc0(n_bytes: gsize): gpointer; cdecl; external;
function g_malloc0_n(n_blocks: gsize; n_block_bytes: gsize): gpointer; cdecl; external;
function g_malloc_n(n_blocks: gsize; n_block_bytes: gsize): gpointer; cdecl; external;
function g_mapped_file_get_contents(AMappedFile: PGMappedFile): Pgchar; cdecl; external;
function g_mapped_file_get_length(AMappedFile: PGMappedFile): gsize; cdecl; external;
function g_mapped_file_new(filename: Pgchar; writable: gboolean): PGMappedFile; cdecl; external;
function g_mapped_file_ref(AMappedFile: PGMappedFile): PGMappedFile; cdecl; external;
function g_markup_collect_attributes(element_name: Pgchar; attribute_names: PPgchar; attribute_values: PPgchar; error: PPGError; first_type: TGMarkupCollectType; first_attr: Pgchar; args: array of const): gboolean; cdecl; external;
function g_markup_error_quark: TGQuark; cdecl; external;
function g_markup_escape_text(text: Pgchar; length: gssize): Pgchar; cdecl; external;
function g_markup_parse_context_end_parse(AMarkupParseContext: PGMarkupParseContext): gboolean; cdecl; external;
function g_markup_parse_context_get_element(AMarkupParseContext: PGMarkupParseContext): Pgchar; cdecl; external;
function g_markup_parse_context_get_element_stack(AMarkupParseContext: PGMarkupParseContext): PGSList; cdecl; external;
function g_markup_parse_context_get_user_data(AMarkupParseContext: PGMarkupParseContext): gpointer; cdecl; external;
function g_markup_parse_context_new(parser: PGMarkupParser; flags: TGMarkupParseFlags; user_data: gpointer; user_data_dnotify: TGDestroyNotify): PGMarkupParseContext; cdecl; external;
function g_markup_parse_context_parse(AMarkupParseContext: PGMarkupParseContext; text: Pgchar; text_len: gssize): gboolean; cdecl; external;
function g_markup_parse_context_pop(AMarkupParseContext: PGMarkupParseContext): gpointer; cdecl; external;
function g_markup_printf_escaped(format: Pgchar; args: array of const): Pgchar; cdecl; external;
function g_markup_vprintf_escaped(format: Pgchar; args: Tva_list): Pgchar; cdecl; external;
function g_match_info_expand_references(AMatchInfo: PGMatchInfo; string_to_expand: Pgchar): Pgchar; cdecl; external;
function g_match_info_fetch(AMatchInfo: PGMatchInfo; match_num: gint): Pgchar; cdecl; external;
function g_match_info_fetch_all(AMatchInfo: PGMatchInfo): PPgchar; cdecl; external;
function g_match_info_fetch_named(AMatchInfo: PGMatchInfo; name: Pgchar): Pgchar; cdecl; external;
function g_match_info_fetch_named_pos(AMatchInfo: PGMatchInfo; name: Pgchar; start_pos: Pgint; end_pos: Pgint): gboolean; cdecl; external;
function g_match_info_fetch_pos(AMatchInfo: PGMatchInfo; match_num: gint; start_pos: Pgint; end_pos: Pgint): gboolean; cdecl; external;
function g_match_info_get_match_count(AMatchInfo: PGMatchInfo): gint; cdecl; external;
function g_match_info_get_regex(AMatchInfo: PGMatchInfo): PGRegex; cdecl; external;
function g_match_info_get_string(AMatchInfo: PGMatchInfo): Pgchar; cdecl; external;
function g_match_info_get_type: TGType; cdecl; external;
function g_match_info_is_partial_match(AMatchInfo: PGMatchInfo): gboolean; cdecl; external;
function g_match_info_matches(AMatchInfo: PGMatchInfo): gboolean; cdecl; external;
function g_match_info_next(AMatchInfo: PGMatchInfo): gboolean; cdecl; external;
function g_match_info_ref(AMatchInfo: PGMatchInfo): PGMatchInfo; cdecl; external;
function g_mem_chunk_alloc(AMemChunk: PGMemChunk): gpointer; cdecl; external;
function g_mem_chunk_alloc0(AMemChunk: PGMemChunk): gpointer; cdecl; external;
function g_mem_chunk_new(name: Pgchar; atom_size: gint; area_size: gsize; type_: gint): PGMemChunk; cdecl; external;
function g_mem_is_system_malloc: gboolean; cdecl; external;
function g_memdup(mem: gpointer; byte_size: guint): gpointer; cdecl; external;
function g_mkdir_with_parents(pathname: Pgchar; mode: gint): gint; cdecl; external;
function g_mkdtemp(tmpl: Pgchar): Pgchar; cdecl; external;
function g_mkdtemp_full(tmpl: Pgchar; mode: gint): Pgchar; cdecl; external;
function g_mkstemp(tmpl: Pgchar): gint; cdecl; external;
function g_mkstemp_full(tmpl: Pgchar; flags: gint; mode: gint): gint; cdecl; external;
function g_node_child_index(ANode: PGNode; data: gpointer): gint; cdecl; external;
function g_node_child_position(ANode: PGNode; child: PGNode): gint; cdecl; external;
function g_node_copy(ANode: PGNode): PGNode; cdecl; external;
function g_node_copy_deep(ANode: PGNode; copy_func: TGCopyFunc; data: gpointer): PGNode; cdecl; external;
function g_node_depth(ANode: PGNode): guint; cdecl; external;
function g_node_find(ANode: PGNode; order: TGTraverseType; flags: TGTraverseFlags; data: gpointer): PGNode; cdecl; external;
function g_node_find_child(ANode: PGNode; flags: TGTraverseFlags; data: gpointer): PGNode; cdecl; external;
function g_node_first_sibling(ANode: PGNode): PGNode; cdecl; external;
function g_node_get_root(ANode: PGNode): PGNode; cdecl; external;
function g_node_insert(ANode: PGNode; position: gint; node: PGNode): PGNode; cdecl; external;
function g_node_insert_after(ANode: PGNode; sibling: PGNode; node: PGNode): PGNode; cdecl; external;
function g_node_insert_before(ANode: PGNode; sibling: PGNode; node: PGNode): PGNode; cdecl; external;
function g_node_is_ancestor(ANode: PGNode; descendant: PGNode): gboolean; cdecl; external;
function g_node_last_child(ANode: PGNode): PGNode; cdecl; external;
function g_node_last_sibling(ANode: PGNode): PGNode; cdecl; external;
function g_node_max_height(ANode: PGNode): guint; cdecl; external;
function g_node_n_children(ANode: PGNode): guint; cdecl; external;
function g_node_n_nodes(ANode: PGNode; flags: TGTraverseFlags): guint; cdecl; external;
function g_node_new(data: gpointer): PGNode; cdecl; external;
function g_node_nth_child(ANode: PGNode; n: guint): PGNode; cdecl; external;
function g_node_prepend(ANode: PGNode; node: PGNode): PGNode; cdecl; external;
function g_once_impl(AOnce: PGOnce; func: TGThreadFunc; arg: gpointer): gpointer; cdecl; external;
function g_once_init_enter(value_location: Pgsize): gboolean; cdecl; external;
function g_once_init_enter_impl(value_location: Pgsize): gboolean; cdecl; external;
function g_option_context_get_description(AOptionContext: PGOptionContext): Pgchar; cdecl; external;
function g_option_context_get_help(AOptionContext: PGOptionContext; main_help: gboolean; group: PGOptionGroup): Pgchar; cdecl; external;
function g_option_context_get_help_enabled(AOptionContext: PGOptionContext): gboolean; cdecl; external;
function g_option_context_get_ignore_unknown_options(AOptionContext: PGOptionContext): gboolean; cdecl; external;
function g_option_context_get_main_group(AOptionContext: PGOptionContext): PGOptionGroup; cdecl; external;
function g_option_context_get_summary(AOptionContext: PGOptionContext): Pgchar; cdecl; external;
function g_option_context_new(parameter_string: Pgchar): PGOptionContext; cdecl; external;
function g_option_context_parse(AOptionContext: PGOptionContext; argc: Pgint; argv: PPPgchar): gboolean; cdecl; external;
function g_option_error_quark: TGQuark; cdecl; external;
function g_option_group_new(name: Pgchar; description: Pgchar; help_description: Pgchar; user_data: gpointer; destroy_: TGDestroyNotify): PGOptionGroup; cdecl; external;
function g_parse_debug_string(string_: Pgchar; keys: PGDebugKey; nkeys: guint): guint; cdecl; external;
function g_path_get_basename(file_name: Pgchar): Pgchar; cdecl; external;
function g_path_get_dirname(file_name: Pgchar): Pgchar; cdecl; external;
function g_path_is_absolute(file_name: Pgchar): gboolean; cdecl; external;
function g_path_skip_root(file_name: Pgchar): Pgchar; cdecl; external;
function g_pattern_match(pspec: PGPatternSpec; string_length: guint; string_: Pgchar; string_reversed: Pgchar): gboolean; cdecl; external;
function g_pattern_match_simple(pattern: Pgchar; string_: Pgchar): gboolean; cdecl; external;
function g_pattern_match_string(pspec: PGPatternSpec; string_: Pgchar): gboolean; cdecl; external;
function g_pattern_spec_equal(APatternSpec: PGPatternSpec; pspec2: PGPatternSpec): gboolean; cdecl; external;
function g_pattern_spec_new(pattern: Pgchar): PGPatternSpec; cdecl; external;
function g_pointer_bit_trylock(address: Pgpointer; lock_bit: gint): gboolean; cdecl; external;
function g_poll(fds: PGPollFD; nfds: guint; timeout: gint): gint; cdecl; external;
function g_printf(format: Pgchar; args: array of const): gint; cdecl; external;
function g_printf_string_upper_bound(format: Pgchar; args: Tva_list): gsize; cdecl; external;
function g_ptr_array_free(array_: Pgpointer; free_seg: gboolean): Pgpointer; cdecl; external;
function g_ptr_array_get_type: TGType; cdecl; external;
function g_ptr_array_new: Pgpointer; cdecl; external;
function g_ptr_array_new_full(reserved_size: guint; element_free_func: TGDestroyNotify): Pgpointer; cdecl; external;
function g_ptr_array_new_with_free_func(element_free_func: TGDestroyNotify): Pgpointer; cdecl; external;
function g_ptr_array_ref(array_: Pgpointer): Pgpointer; cdecl; external;
function g_ptr_array_remove(array_: Pgpointer; data: gpointer): gboolean; cdecl; external;
function g_ptr_array_remove_fast(array_: Pgpointer; data: gpointer): gboolean; cdecl; external;
function g_ptr_array_remove_index(array_: Pgpointer; index_: guint): gpointer; cdecl; external;
function g_ptr_array_remove_index_fast(array_: Pgpointer; index_: guint): gpointer; cdecl; external;
function g_ptr_array_sized_new(reserved_size: guint): Pgpointer; cdecl; external;
function g_quark_from_static_string(string_: Pgchar): TGQuark; cdecl; external;
function g_quark_from_string(string_: Pgchar): TGQuark; cdecl; external;
function g_quark_to_string(quark: TGQuark): Pgchar; cdecl; external;
function g_quark_try_string(string_: Pgchar): TGQuark; cdecl; external;
function g_queue_copy(AQueue: PGQueue): PGQueue; cdecl; external;
function g_queue_find(AQueue: PGQueue; data: gpointer): PGList; cdecl; external;
function g_queue_find_custom(AQueue: PGQueue; data: gpointer; func: TGCompareFunc): PGList; cdecl; external;
function g_queue_get_length(AQueue: PGQueue): guint; cdecl; external;
function g_queue_index(AQueue: PGQueue; data: gpointer): gint; cdecl; external;
function g_queue_is_empty(AQueue: PGQueue): gboolean; cdecl; external;
function g_queue_link_index(AQueue: PGQueue; link_: PGList): gint; cdecl; external;
function g_queue_new: PGQueue; cdecl; external;
function g_queue_peek_head(AQueue: PGQueue): gpointer; cdecl; external;
function g_queue_peek_head_link(AQueue: PGQueue): PGList; cdecl; external;
function g_queue_peek_nth(AQueue: PGQueue; n: guint): gpointer; cdecl; external;
function g_queue_peek_nth_link(AQueue: PGQueue; n: guint): PGList; cdecl; external;
function g_queue_peek_tail(AQueue: PGQueue): gpointer; cdecl; external;
function g_queue_peek_tail_link(AQueue: PGQueue): PGList; cdecl; external;
function g_queue_pop_head(AQueue: PGQueue): gpointer; cdecl; external;
function g_queue_pop_head_link(AQueue: PGQueue): PGList; cdecl; external;
function g_queue_pop_nth(AQueue: PGQueue; n: guint): gpointer; cdecl; external;
function g_queue_pop_nth_link(AQueue: PGQueue; n: guint): PGList; cdecl; external;
function g_queue_pop_tail(AQueue: PGQueue): gpointer; cdecl; external;
function g_queue_pop_tail_link(AQueue: PGQueue): PGList; cdecl; external;
function g_queue_remove(AQueue: PGQueue; data: gpointer): gboolean; cdecl; external;
function g_queue_remove_all(AQueue: PGQueue; data: gpointer): guint; cdecl; external;
function g_rand_copy(ARand: PGRand): PGRand; cdecl; external;
function g_rand_double(ARand: PGRand): gdouble; cdecl; external;
function g_rand_double_range(ARand: PGRand; begin_: gdouble; end_: gdouble): gdouble; cdecl; external;
function g_rand_int(ARand: PGRand): guint32; cdecl; external;
function g_rand_int_range(ARand: PGRand; begin_: gint32; end_: gint32): gint32; cdecl; external;
function g_rand_new: PGRand; cdecl; external;
function g_rand_new_with_seed(seed: guint32): PGRand; cdecl; external;
function g_rand_new_with_seed_array(seed: Pguint32; seed_length: guint): PGRand; cdecl; external;
function g_random_double: gdouble; cdecl; external;
function g_random_double_range(begin_: gdouble; end_: gdouble): gdouble; cdecl; external;
function g_random_int: guint32; cdecl; external;
function g_random_int_range(begin_: gint32; end_: gint32): gint32; cdecl; external;
function g_realloc(mem: gpointer; n_bytes: gsize): gpointer; cdecl; external;
function g_realloc_n(mem: gpointer; n_blocks: gsize; n_block_bytes: gsize): gpointer; cdecl; external;
function g_regex_check_replacement(replacement: Pgchar; has_references: Pgboolean): gboolean; cdecl; external;
function g_regex_error_quark: TGQuark; cdecl; external;
function g_regex_escape_nul(string_: Pgchar; length: gint): Pgchar; cdecl; external;
function g_regex_escape_string(string_: Pgchar; length: gint): Pgchar; cdecl; external;
function g_regex_get_capture_count(ARegex: PGRegex): gint; cdecl; external;
function g_regex_get_compile_flags(ARegex: PGRegex): TGRegexCompileFlags; cdecl; external;
function g_regex_get_match_flags(ARegex: PGRegex): TGRegexMatchFlags; cdecl; external;
function g_regex_get_max_backref(ARegex: PGRegex): gint; cdecl; external;
function g_regex_get_pattern(ARegex: PGRegex): Pgchar; cdecl; external;
function g_regex_get_string_number(ARegex: PGRegex; name: Pgchar): gint; cdecl; external;
function g_regex_get_type: TGType; cdecl; external;
function g_regex_match(ARegex: PGRegex; string_: Pgchar; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl; external;
function g_regex_match_all(ARegex: PGRegex; string_: Pgchar; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl; external;
function g_regex_match_all_full(ARegex: PGRegex; string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl; external;
function g_regex_match_full(ARegex: PGRegex; string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl; external;
function g_regex_match_simple(pattern: Pgchar; string_: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): gboolean; cdecl; external;
function g_regex_new(pattern: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): PGRegex; cdecl; external;
function g_regex_ref(ARegex: PGRegex): PGRegex; cdecl; external;
function g_regex_replace(ARegex: PGRegex; string_: Pgchar; string_len: gssize; start_position: gint; replacement: Pgchar; match_options: TGRegexMatchFlags): Pgchar; cdecl; external;
function g_regex_replace_eval(ARegex: PGRegex; string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; eval: TGRegexEvalCallback; user_data: gpointer): Pgchar; cdecl; external;
function g_regex_replace_literal(ARegex: PGRegex; string_: Pgchar; string_len: gssize; start_position: gint; replacement: Pgchar; match_options: TGRegexMatchFlags): Pgchar; cdecl; external;
function g_regex_split(ARegex: PGRegex; string_: Pgchar; match_options: TGRegexMatchFlags): PPgchar; cdecl; external;
function g_regex_split_full(ARegex: PGRegex; string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; max_tokens: gint): PPgchar; cdecl; external;
function g_regex_split_simple(pattern: Pgchar; string_: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): PPgchar; cdecl; external;
function g_rmdir(filename: Pgchar): gint; cdecl; external;
function g_scanner_cur_line(AScanner: PGScanner): guint; cdecl; external;
function g_scanner_cur_position(AScanner: PGScanner): guint; cdecl; external;
function g_scanner_cur_token(AScanner: PGScanner): TGTokenType; cdecl; external;
function g_scanner_cur_value(AScanner: PGScanner): TGTokenValue; cdecl; external;
function g_scanner_eof(AScanner: PGScanner): gboolean; cdecl; external;
function g_scanner_get_next_token(AScanner: PGScanner): TGTokenType; cdecl; external;
function g_scanner_lookup_symbol(AScanner: PGScanner; symbol: Pgchar): gpointer; cdecl; external;
function g_scanner_new(config_templ: PGScannerConfig): PGScanner; cdecl; external;
function g_scanner_peek_next_token(AScanner: PGScanner): TGTokenType; cdecl; external;
function g_scanner_scope_lookup_symbol(AScanner: PGScanner; scope_id: guint; symbol: Pgchar): gpointer; cdecl; external;
function g_scanner_set_scope(AScanner: PGScanner; scope_id: guint): guint; cdecl; external;
function g_sequence_append(ASequence: PGSequence; data: gpointer): PGSequenceIter; cdecl; external;
function g_sequence_get(iter: PGSequenceIter): gpointer; cdecl; external;
function g_sequence_get_begin_iter(ASequence: PGSequence): PGSequenceIter; cdecl; external;
function g_sequence_get_end_iter(ASequence: PGSequence): PGSequenceIter; cdecl; external;
function g_sequence_get_iter_at_pos(ASequence: PGSequence; pos: gint): PGSequenceIter; cdecl; external;
function g_sequence_get_length(ASequence: PGSequence): gint; cdecl; external;
function g_sequence_insert_before(iter: PGSequenceIter; data: gpointer): PGSequenceIter; cdecl; external;
function g_sequence_insert_sorted(ASequence: PGSequence; data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl; external;
function g_sequence_insert_sorted_iter(ASequence: PGSequence; data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl; external;
function g_sequence_iter_compare(ASequenceIter: PGSequenceIter; b: PGSequenceIter): gint; cdecl; external;
function g_sequence_iter_get_position(ASequenceIter: PGSequenceIter): gint; cdecl; external;
function g_sequence_iter_get_sequence(ASequenceIter: PGSequenceIter): PGSequence; cdecl; external;
function g_sequence_iter_is_begin(ASequenceIter: PGSequenceIter): gboolean; cdecl; external;
function g_sequence_iter_is_end(ASequenceIter: PGSequenceIter): gboolean; cdecl; external;
function g_sequence_iter_move(ASequenceIter: PGSequenceIter; delta: gint): PGSequenceIter; cdecl; external;
function g_sequence_iter_next(ASequenceIter: PGSequenceIter): PGSequenceIter; cdecl; external;
function g_sequence_iter_prev(ASequenceIter: PGSequenceIter): PGSequenceIter; cdecl; external;
function g_sequence_lookup(ASequence: PGSequence; data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl; external;
function g_sequence_lookup_iter(ASequence: PGSequence; data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl; external;
function g_sequence_new(data_destroy: TGDestroyNotify): PGSequence; cdecl; external;
function g_sequence_prepend(ASequence: PGSequence; data: gpointer): PGSequenceIter; cdecl; external;
function g_sequence_range_get_midpoint(begin_: PGSequenceIter; end_: PGSequenceIter): PGSequenceIter; cdecl; external;
function g_sequence_search(ASequence: PGSequence; data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl; external;
function g_sequence_search_iter(ASequence: PGSequence; data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl; external;
function g_set_print_handler(func: TGPrintFunc): TGPrintFunc; cdecl; external;
function g_set_printerr_handler(func: TGPrintFunc): TGPrintFunc; cdecl; external;
function g_setenv(variable: Pgchar; value: Pgchar; overwrite: gboolean): gboolean; cdecl; external;
function g_shell_error_quark: TGQuark; cdecl; external;
function g_shell_parse_argv(command_line: Pgchar; argcp: Pgint; argvp: PPPgchar): gboolean; cdecl; external;
function g_shell_quote(unquoted_string: Pgchar): Pgchar; cdecl; external;
function g_shell_unquote(quoted_string: Pgchar): Pgchar; cdecl; external;
function g_slice_alloc(block_size: gsize): gpointer; cdecl; external;
function g_slice_alloc0(block_size: gsize): gpointer; cdecl; external;
function g_slice_copy(block_size: gsize; mem_block: gpointer): gpointer; cdecl; external;
function g_slice_get_config(ckey: TGSliceConfig): gint64; cdecl; external;
function g_slice_get_config_state(ckey: TGSliceConfig; address: gint64; n_values: Pguint): Pgint64; cdecl; external;
function g_slist_alloc: PGSList; cdecl; external;
function g_slist_append(list: PGSList; data: gpointer): PGSList; cdecl; external;
function g_slist_concat(list1: PGSList; list2: PGSList): PGSList; cdecl; external;
function g_slist_copy(list: PGSList): PGSList; cdecl; external;
function g_slist_delete_link(list: PGSList; link_: PGSList): PGSList; cdecl; external;
function g_slist_find(list: PGSList; data: gpointer): PGSList; cdecl; external;
function g_slist_find_custom(list: PGSList; data: gpointer; func: TGCompareFunc): PGSList; cdecl; external;
function g_slist_index(list: PGSList; data: gpointer): gint; cdecl; external;
function g_slist_insert(list: PGSList; data: gpointer; position: gint): PGSList; cdecl; external;
function g_slist_insert_before(slist: PGSList; sibling: PGSList; data: gpointer): PGSList; cdecl; external;
function g_slist_insert_sorted(list: PGSList; data: gpointer; func: TGCompareFunc): PGSList; cdecl; external;
function g_slist_insert_sorted_with_data(list: PGSList; data: gpointer; func: TGCompareDataFunc; user_data: gpointer): PGSList; cdecl; external;
function g_slist_last(list: PGSList): PGSList; cdecl; external;
function g_slist_length(list: PGSList): guint; cdecl; external;
function g_slist_nth(list: PGSList; n: guint): PGSList; cdecl; external;
function g_slist_nth_data(list: PGSList; n: guint): gpointer; cdecl; external;
function g_slist_position(list: PGSList; llink: PGSList): gint; cdecl; external;
function g_slist_prepend(list: PGSList; data: gpointer): PGSList; cdecl; external;
function g_slist_remove(list: PGSList; data: gpointer): PGSList; cdecl; external;
function g_slist_remove_all(list: PGSList; data: gpointer): PGSList; cdecl; external;
function g_slist_remove_link(list: PGSList; link_: PGSList): PGSList; cdecl; external;
function g_slist_reverse(list: PGSList): PGSList; cdecl; external;
function g_slist_sort(list: PGSList; compare_func: TGCompareFunc): PGSList; cdecl; external;
function g_slist_sort_with_data(list: PGSList; compare_func: TGCompareDataFunc; user_data: gpointer): PGSList; cdecl; external;
function g_snprintf(string_: Pgchar; n: gulong; format: Pgchar; args: array of const): gint; cdecl; external;
function g_source_attach(ASource: PGSource; context: PGMainContext): guint; cdecl; external;
function g_source_get_can_recurse(ASource: PGSource): gboolean; cdecl; external;
function g_source_get_context(ASource: PGSource): PGMainContext; cdecl; external;
function g_source_get_id(ASource: PGSource): guint; cdecl; external;
function g_source_get_name(ASource: PGSource): Pgchar; cdecl; external;
function g_source_get_priority(ASource: PGSource): gint; cdecl; external;
function g_source_get_time(ASource: PGSource): gint64; cdecl; external;
function g_source_is_destroyed(ASource: PGSource): gboolean; cdecl; external;
function g_source_new(source_funcs: PGSourceFuncs; struct_size: guint): PGSource; cdecl; external;
function g_source_ref(ASource: PGSource): PGSource; cdecl; external;
function g_source_remove(tag: guint): gboolean; cdecl; external;
function g_source_remove_by_funcs_user_data(funcs: PGSourceFuncs; user_data: gpointer): gboolean; cdecl; external;
function g_source_remove_by_user_data(user_data: gpointer): gboolean; cdecl; external;
function g_spaced_primes_closest(num: guint): guint; cdecl; external;
function g_spawn_async(working_directory: Pgchar; argv: PPgchar; envp: PPgchar; flags: TGSpawnFlags; child_setup: TGSpawnChildSetupFunc; user_data: gpointer; child_pid: PGPid): gboolean; cdecl; external;
function g_spawn_async_with_pipes(working_directory: Pgchar; argv: PPgchar; envp: PPgchar; flags: TGSpawnFlags; child_setup: TGSpawnChildSetupFunc; user_data: gpointer; child_pid: PGPid; standard_input: Pgint; standard_output: Pgint; standard_error: Pgint): gboolean; cdecl; external;
function g_spawn_command_line_async(command_line: Pgchar): gboolean; cdecl; external;
function g_spawn_command_line_sync(command_line: Pgchar; standard_output: PPgchar; standard_error: PPgchar; exit_status: Pgint): gboolean; cdecl; external;
function g_spawn_error_quark: TGQuark; cdecl; external;
function g_spawn_sync(working_directory: Pgchar; argv: PPgchar; envp: PPgchar; flags: TGSpawnFlags; child_setup: TGSpawnChildSetupFunc; user_data: gpointer; standard_output: PPgchar; standard_error: PPgchar; exit_status: Pgint): gboolean; cdecl; external;
function g_sprintf(string_: Pgchar; format: Pgchar; args: array of const): gint; cdecl; external;
function g_static_mutex_get_mutex_impl(mutex: PPGMutex): PGMutex; cdecl; external;
function g_static_private_get(AStaticPrivate: PGStaticPrivate): gpointer; cdecl; external;
function g_static_rec_mutex_trylock(AStaticRecMutex: PGStaticRecMutex): gboolean; cdecl; external;
function g_static_rec_mutex_unlock_full(AStaticRecMutex: PGStaticRecMutex): guint; cdecl; external;
function g_static_rw_lock_reader_trylock(AStaticRWLock: PGStaticRWLock): gboolean; cdecl; external;
function g_static_rw_lock_writer_trylock(AStaticRWLock: PGStaticRWLock): gboolean; cdecl; external;
function g_stpcpy(dest: Pgchar; src: Pgchar): Pgchar; cdecl; external;
function g_str_equal(v1: gpointer; v2: gpointer): gboolean; cdecl; external;
function g_str_has_prefix(str: Pgchar; prefix: Pgchar): gboolean; cdecl; external;
function g_str_has_suffix(str: Pgchar; suffix: Pgchar): gboolean; cdecl; external;
function g_str_hash(v: gpointer): guint; cdecl; external;
function g_strcanon(string_: Pgchar; valid_chars: Pgchar; substitutor: gchar): Pgchar; cdecl; external;
function g_strcasecmp(s1: Pgchar; s2: Pgchar): gint; cdecl; external;
function g_strchomp(string_: Pgchar): Pgchar; cdecl; external;
function g_strchug(string_: Pgchar): Pgchar; cdecl; external;
function g_strcmp0(str1: Pgchar; str2: Pgchar): gint; cdecl; external;
function g_strcompress(source: Pgchar): Pgchar; cdecl; external;
function g_strconcat(string1: Pgchar; args: array of const): Pgchar; cdecl; external;
function g_strdelimit(string_: Pgchar; delimiters: Pgchar; new_delimiter: gchar): Pgchar; cdecl; external;
function g_strdown(string_: Pgchar): Pgchar; cdecl; external;
function g_strdup(str: Pgchar): Pgchar; cdecl; external;
function g_strdup_printf(format: Pgchar; args: array of const): Pgchar; cdecl; external;
function g_strdup_vprintf(format: Pgchar; args: Tva_list): Pgchar; cdecl; external;
function g_strdupv(str_array: PPgchar): PPgchar; cdecl; external;
function g_strerror(errnum: gint): Pgchar; cdecl; external;
function g_strescape(source: Pgchar; exceptions: Pgchar): Pgchar; cdecl; external;
function g_string_append(AString: PGString; val: Pgchar): PGString; cdecl; external;
function g_string_append_c(AString: PGString; c: gchar): PGString; cdecl; external;
function g_string_append_len(AString: PGString; val: Pgchar; len: gssize): PGString; cdecl; external;
function g_string_append_unichar(AString: PGString; wc: gunichar): PGString; cdecl; external;
function g_string_append_uri_escaped(AString: PGString; unescaped: Pgchar; reserved_chars_allowed: Pgchar; allow_utf8: gboolean): PGString; cdecl; external;
function g_string_ascii_down(AString: PGString): PGString; cdecl; external;
function g_string_ascii_up(AString: PGString): PGString; cdecl; external;
function g_string_assign(AString: PGString; rval: Pgchar): PGString; cdecl; external;
function g_string_chunk_insert(AStringChunk: PGStringChunk; string_: Pgchar): Pgchar; cdecl; external;
function g_string_chunk_insert_const(AStringChunk: PGStringChunk; string_: Pgchar): Pgchar; cdecl; external;
function g_string_chunk_insert_len(AStringChunk: PGStringChunk; string_: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_string_chunk_new(size: gsize): PGStringChunk; cdecl; external;
function g_string_down(AString: PGString): PGString; cdecl; external;
function g_string_equal(AString: PGString; v2: PGString): gboolean; cdecl; external;
function g_string_erase(AString: PGString; pos: gssize; len: gssize): PGString; cdecl; external;
function g_string_free(AString: PGString; free_segment: gboolean): Pgchar; cdecl; external;
function g_string_hash(AString: PGString): guint; cdecl; external;
function g_string_insert(AString: PGString; pos: gssize; val: Pgchar): PGString; cdecl; external;
function g_string_insert_c(AString: PGString; pos: gssize; c: gchar): PGString; cdecl; external;
function g_string_insert_len(AString: PGString; pos: gssize; val: Pgchar; len: gssize): PGString; cdecl; external;
function g_string_insert_unichar(AString: PGString; pos: gssize; wc: gunichar): PGString; cdecl; external;
function g_string_new(init: Pgchar): PGString; cdecl; external;
function g_string_new_len(init: Pgchar; len: gssize): PGString; cdecl; external;
function g_string_overwrite(AString: PGString; pos: gsize; val: Pgchar): PGString; cdecl; external;
function g_string_overwrite_len(AString: PGString; pos: gsize; val: Pgchar; len: gssize): PGString; cdecl; external;
function g_string_prepend(AString: PGString; val: Pgchar): PGString; cdecl; external;
function g_string_prepend_c(AString: PGString; c: gchar): PGString; cdecl; external;
function g_string_prepend_len(AString: PGString; val: Pgchar; len: gssize): PGString; cdecl; external;
function g_string_prepend_unichar(AString: PGString; wc: gunichar): PGString; cdecl; external;
function g_string_set_size(AString: PGString; len: gsize): PGString; cdecl; external;
function g_string_sized_new(dfl_size: gsize): PGString; cdecl; external;
function g_string_truncate(AString: PGString; len: gsize): PGString; cdecl; external;
function g_string_up(AString: PGString): PGString; cdecl; external;
function g_strip_context(msgid: Pgchar; msgval: Pgchar): Pgchar; cdecl; external;
function g_strjoin(separator: Pgchar; args: array of const): Pgchar; cdecl; external;
function g_strjoinv(separator: Pgchar; str_array: PPgchar): Pgchar; cdecl; external;
function g_strlcat(dest: Pgchar; src: Pgchar; dest_size: gsize): gsize; cdecl; external;
function g_strlcpy(dest: Pgchar; src: Pgchar; dest_size: gsize): gsize; cdecl; external;
function g_strncasecmp(s1: Pgchar; s2: Pgchar; n: guint): gint; cdecl; external;
function g_strndup(str: Pgchar; n: gsize): Pgchar; cdecl; external;
function g_strnfill(length: gsize; fill_char: gchar): Pgchar; cdecl; external;
function g_strreverse(string_: Pgchar): Pgchar; cdecl; external;
function g_strrstr(haystack: Pgchar; needle: Pgchar): Pgchar; cdecl; external;
function g_strrstr_len(haystack: Pgchar; haystack_len: gssize; needle: Pgchar): Pgchar; cdecl; external;
function g_strsignal(signum: gint): Pgchar; cdecl; external;
function g_strsplit(string_: Pgchar; delimiter: Pgchar; max_tokens: gint): PPgchar; cdecl; external;
function g_strsplit_set(string_: Pgchar; delimiters: Pgchar; max_tokens: gint): PPgchar; cdecl; external;
function g_strstr_len(haystack: Pgchar; haystack_len: gssize; needle: Pgchar): Pgchar; cdecl; external;
function g_strtod(nptr: Pgchar; endptr: PPgchar): gdouble; cdecl; external;
function g_strup(string_: Pgchar): Pgchar; cdecl; external;
function g_strv_get_type: TGType; cdecl; external;
function g_strv_length(str_array: PPgchar): guint; cdecl; external;
function g_test_create_case(test_name: Pgchar; data_size: gsize; test_data: gpointer; data_setup: TGTestFixtureFunc; data_test: TGTestFixtureFunc; data_teardown: TGTestFixtureFunc): PGTestCase; cdecl; external;
function g_test_create_suite(suite_name: Pgchar): PGTestSuite; cdecl; external;
function g_test_get_root: PGTestSuite; cdecl; external;
function g_test_log_buffer_new: PGTestLogBuffer; cdecl; external;
function g_test_log_buffer_pop(ATestLogBuffer: PGTestLogBuffer): PGTestLogMsg; cdecl; external;
function g_test_log_type_name(log_type: TGTestLogType): Pgchar; cdecl; external;
function g_test_rand_double: gdouble; cdecl; external;
function g_test_rand_double_range(range_start: gdouble; range_end: gdouble): gdouble; cdecl; external;
function g_test_rand_int: gint32; cdecl; external;
function g_test_rand_int_range(begin_: gint32; end_: gint32): gint32; cdecl; external;
function g_test_run: gint; cdecl; external;
function g_test_run_suite(suite: PGTestSuite): gint; cdecl; external;
function g_test_timer_elapsed: gdouble; cdecl; external;
function g_test_timer_last: gdouble; cdecl; external;
function g_test_trap_fork(usec_timeout: guint64; test_trap_flags: TGTestTrapFlags): gboolean; cdecl; external;
function g_test_trap_has_passed: gboolean; cdecl; external;
function g_test_trap_reached_timeout: gboolean; cdecl; external;
function g_thread_create_full(func: TGThreadFunc; data: gpointer; stack_size: gulong; joinable: gboolean; bound: gboolean; priority: TGThreadPriority): PGThread; cdecl; external;
function g_thread_error_quark: TGQuark; cdecl; external;
function g_thread_get_initialized: gboolean; cdecl; external;
function g_thread_join(AThread: PGThread): gpointer; cdecl; external;
function g_thread_pool_get_max_idle_time: guint; cdecl; external;
function g_thread_pool_get_max_threads(AThreadPool: PGThreadPool): gint; cdecl; external;
function g_thread_pool_get_max_unused_threads: gint; cdecl; external;
function g_thread_pool_get_num_threads(AThreadPool: PGThreadPool): guint; cdecl; external;
function g_thread_pool_get_num_unused_threads: guint; cdecl; external;
function g_thread_pool_new(func: TGFunc; user_data: gpointer; max_threads: gint; exclusive: gboolean): PGThreadPool; cdecl; external;
function g_thread_pool_unprocessed(AThreadPool: PGThreadPool): guint; cdecl; external;
function g_thread_self: PGThread; cdecl; external;
function g_time_val_from_iso8601(iso_date: Pgchar; time_: PGTimeVal): gboolean; cdecl; external;
function g_time_val_to_iso8601(ATimeVal: PGTimeVal): Pgchar; cdecl; external;
function g_time_zone_adjust_time(ATimeZone: PGTimeZone; type_: TGTimeType; time_: Pgint64): gint; cdecl; external;
function g_time_zone_find_interval(ATimeZone: PGTimeZone; type_: TGTimeType; time_: gint64): gint; cdecl; external;
function g_time_zone_get_abbreviation(ATimeZone: PGTimeZone; interval: gint): Pgchar; cdecl; external;
function g_time_zone_get_offset(ATimeZone: PGTimeZone; interval: gint): gint32; cdecl; external;
function g_time_zone_is_dst(ATimeZone: PGTimeZone; interval: gint): gboolean; cdecl; external;
function g_time_zone_new(identifier: Pgchar): PGTimeZone; cdecl; external;
function g_time_zone_new_local: PGTimeZone; cdecl; external;
function g_time_zone_new_utc: PGTimeZone; cdecl; external;
function g_time_zone_ref(ATimeZone: PGTimeZone): PGTimeZone; cdecl; external;
function g_timeout_add(interval: guint; function_: TGSourceFunc; data: gpointer): guint; cdecl; external;
function g_timeout_add_full(priority: gint; interval: guint; function_: TGSourceFunc; data: gpointer; notify: TGDestroyNotify): guint; cdecl; external;
function g_timeout_add_seconds(interval: guint; function_: TGSourceFunc; data: gpointer): guint; cdecl; external;
function g_timeout_add_seconds_full(priority: gint; interval: guint; function_: TGSourceFunc; data: gpointer; notify: TGDestroyNotify): guint; cdecl; external;
function g_timeout_source_new(interval: guint): PGSource; cdecl; external;
function g_timeout_source_new_seconds(interval: guint): PGSource; cdecl; external;
function g_timer_elapsed(ATimer: PGTimer; microseconds: Pgulong): gdouble; cdecl; external;
function g_timer_new: PGTimer; cdecl; external;
function g_trash_stack_height(stack_p: PPGTrashStack): guint; cdecl; external;
function g_trash_stack_peek(stack_p: PPGTrashStack): gpointer; cdecl; external;
function g_trash_stack_pop(stack_p: PPGTrashStack): gpointer; cdecl; external;
function g_tree_height(ATree: PGTree): gint; cdecl; external;
function g_tree_lookup(ATree: PGTree; key: gpointer): gpointer; cdecl; external;
function g_tree_lookup_extended(ATree: PGTree; lookup_key: gpointer; orig_key: Pgpointer; value: Pgpointer): gboolean; cdecl; external;
function g_tree_new(key_compare_func: TGCompareFunc): PGTree; cdecl; external;
function g_tree_new_full(key_compare_func: TGCompareDataFunc; key_compare_data: gpointer; key_destroy_func: TGDestroyNotify; value_destroy_func: TGDestroyNotify): PGTree; cdecl; external;
function g_tree_new_with_data(key_compare_func: TGCompareDataFunc; key_compare_data: gpointer): PGTree; cdecl; external;
function g_tree_nnodes(ATree: PGTree): gint; cdecl; external;
function g_tree_ref(ATree: PGTree): PGTree; cdecl; external;
function g_tree_remove(ATree: PGTree; key: gpointer): gboolean; cdecl; external;
function g_tree_search(ATree: PGTree; search_func: TGCompareFunc; user_data: gpointer): gpointer; cdecl; external;
function g_tree_steal(ATree: PGTree; key: gpointer): gboolean; cdecl; external;
function g_try_malloc(n_bytes: gsize): gpointer; cdecl; external;
function g_try_malloc0(n_bytes: gsize): gpointer; cdecl; external;
function g_try_malloc0_n(n_blocks: gsize; n_block_bytes: gsize): gpointer; cdecl; external;
function g_try_malloc_n(n_blocks: gsize; n_block_bytes: gsize): gpointer; cdecl; external;
function g_try_realloc(mem: gpointer; n_bytes: gsize): gpointer; cdecl; external;
function g_try_realloc_n(mem: gpointer; n_blocks: gsize; n_block_bytes: gsize): gpointer; cdecl; external;
function g_ucs4_to_utf16(str: Pgunichar; len: glong; items_read: Pglong; items_written: Pglong): Pguint16; cdecl; external;
function g_ucs4_to_utf8(str: Pgunichar; len: glong; items_read: Pglong; items_written: Pglong): Pgchar; cdecl; external;
function g_unichar_break_type(c: gunichar): TGUnicodeBreakType; cdecl; external;
function g_unichar_combining_class(uc: gunichar): gint; cdecl; external;
function g_unichar_compose(a: gunichar; b: gunichar; ch: Pgunichar): gboolean; cdecl; external;
function g_unichar_decompose(ch: gunichar; a: Pgunichar; b: Pgunichar): gboolean; cdecl; external;
function g_unichar_digit_value(c: gunichar): gint; cdecl; external;
function g_unichar_fully_decompose(ch: gunichar; compat: gboolean; result_: Pgunichar; result_len: gsize): gsize; cdecl; external;
function g_unichar_get_mirror_char(ch: gunichar; mirrored_ch: Pgunichar): gboolean; cdecl; external;
function g_unichar_get_script(ch: gunichar): TGUnicodeScript; cdecl; external;
function g_unichar_isalnum(c: gunichar): gboolean; cdecl; external;
function g_unichar_isalpha(c: gunichar): gboolean; cdecl; external;
function g_unichar_iscntrl(c: gunichar): gboolean; cdecl; external;
function g_unichar_isdefined(c: gunichar): gboolean; cdecl; external;
function g_unichar_isdigit(c: gunichar): gboolean; cdecl; external;
function g_unichar_isgraph(c: gunichar): gboolean; cdecl; external;
function g_unichar_islower(c: gunichar): gboolean; cdecl; external;
function g_unichar_ismark(c: gunichar): gboolean; cdecl; external;
function g_unichar_isprint(c: gunichar): gboolean; cdecl; external;
function g_unichar_ispunct(c: gunichar): gboolean; cdecl; external;
function g_unichar_isspace(c: gunichar): gboolean; cdecl; external;
function g_unichar_istitle(c: gunichar): gboolean; cdecl; external;
function g_unichar_isupper(c: gunichar): gboolean; cdecl; external;
function g_unichar_iswide(c: gunichar): gboolean; cdecl; external;
function g_unichar_iswide_cjk(c: gunichar): gboolean; cdecl; external;
function g_unichar_isxdigit(c: gunichar): gboolean; cdecl; external;
function g_unichar_iszerowidth(c: gunichar): gboolean; cdecl; external;
function g_unichar_to_utf8(c: gunichar; outbuf: Pgchar): gint; cdecl; external;
function g_unichar_tolower(c: gunichar): gunichar; cdecl; external;
function g_unichar_totitle(c: gunichar): gunichar; cdecl; external;
function g_unichar_toupper(c: gunichar): gunichar; cdecl; external;
function g_unichar_type(c: gunichar): TGUnicodeType; cdecl; external;
function g_unichar_validate(ch: gunichar): gboolean; cdecl; external;
function g_unichar_xdigit_value(c: gunichar): gint; cdecl; external;
function g_unicode_canonical_decomposition(ch: gunichar; result_len: Pgsize): Pgunichar; cdecl; external;
function g_unicode_script_from_iso15924(iso15924: guint32): TGUnicodeScript; cdecl; external;
function g_unicode_script_to_iso15924(script: TGUnicodeScript): guint32; cdecl; external;
function g_unlink(filename: Pgchar): gint; cdecl; external;
function g_uri_escape_string(unescaped: Pgchar; reserved_chars_allowed: Pgchar; allow_utf8: gboolean): Pgchar; cdecl; external;
function g_uri_list_extract_uris(uri_list: Pgchar): PPgchar; cdecl; external;
function g_uri_parse_scheme(uri: Pgchar): Pgchar; cdecl; external;
function g_uri_unescape_segment(escaped_string: Pgchar; escaped_string_end: Pgchar; illegal_characters: Pgchar): Pgchar; cdecl; external;
function g_uri_unescape_string(escaped_string: Pgchar; illegal_characters: Pgchar): Pgchar; cdecl; external;
function g_utf16_to_ucs4(str: Pguint16; len: glong; items_read: Pglong; items_written: Pglong): Pgunichar; cdecl; external;
function g_utf16_to_utf8(str: Pguint16; len: glong; items_read: Pglong; items_written: Pglong): Pgchar; cdecl; external;
function g_utf8_casefold(str: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_utf8_collate(str1: Pgchar; str2: Pgchar): gint; cdecl; external;
function g_utf8_collate_key(str: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_utf8_collate_key_for_filename(str: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_utf8_find_next_char(p: Pgchar; end_: Pgchar): Pgchar; cdecl; external;
function g_utf8_find_prev_char(str: Pgchar; p: Pgchar): Pgchar; cdecl; external;
function g_utf8_get_char(p: Pgchar): gunichar; cdecl; external;
function g_utf8_get_char_validated(p: Pgchar; max_len: gssize): gunichar; cdecl; external;
function g_utf8_normalize(str: Pgchar; len: gssize; mode: TGNormalizeMode): Pgchar; cdecl; external;
function g_utf8_offset_to_pointer(str: Pgchar; offset: glong): Pgchar; cdecl; external;
function g_utf8_pointer_to_offset(str: Pgchar; pos: Pgchar): glong; cdecl; external;
function g_utf8_prev_char(p: Pgchar): Pgchar; cdecl; external;
function g_utf8_strchr(p: Pgchar; len: gssize; c: gunichar): Pgchar; cdecl; external;
function g_utf8_strdown(str: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_utf8_strlen(p: Pgchar; max: gssize): glong; cdecl; external;
function g_utf8_strncpy(dest: Pgchar; src: Pgchar; n: gsize): Pgchar; cdecl; external;
function g_utf8_strrchr(p: Pgchar; len: gssize; c: gunichar): Pgchar; cdecl; external;
function g_utf8_strreverse(str: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_utf8_strup(str: Pgchar; len: gssize): Pgchar; cdecl; external;
function g_utf8_substring(str: Pgchar; start_pos: glong; end_pos: glong): Pgchar; cdecl; external;
function g_utf8_to_ucs4(str: Pgchar; len: glong; items_read: Pglong; items_written: Pglong): Pgunichar; cdecl; external;
function g_utf8_to_ucs4_fast(str: Pgchar; len: glong; items_written: Pglong): Pgunichar; cdecl; external;
function g_utf8_to_utf16(str: Pgchar; len: glong; items_read: Pglong; items_written: Pglong): Pguint16; cdecl; external;
function g_utf8_validate(str: Pgchar; max_len: gssize; end_: PPgchar): gboolean; cdecl; external;
function g_variant_builder_end(AVariantBuilder: PGVariantBuilder): PGVariant; cdecl; external;
function g_variant_builder_get_type: TGType; cdecl; external;
function g_variant_builder_new(type_: PGVariantType): PGVariantBuilder; cdecl; external;
function g_variant_builder_ref(AVariantBuilder: PGVariantBuilder): PGVariantBuilder; cdecl; external;
function g_variant_byteswap(AVariant: PGVariant): PGVariant; cdecl; external;
function g_variant_classify(AVariant: PGVariant): TGVariantClass; cdecl; external;
function g_variant_compare(AVariant: PGVariant; two: TGVariant): gint; cdecl; external;
function g_variant_dup_bytestring(AVariant: PGVariant; length: Pgsize): Pgchar; cdecl; external;
function g_variant_dup_bytestring_array(AVariant: PGVariant; length: Pgsize): PPgchar; cdecl; external;
function g_variant_dup_objv(AVariant: PGVariant; length: Pgsize): PPgchar; cdecl; external;
function g_variant_dup_string(AVariant: PGVariant; length: Pgsize): Pgchar; cdecl; external;
function g_variant_dup_strv(AVariant: PGVariant; length: Pgsize): PPgchar; cdecl; external;
function g_variant_equal(AVariant: PGVariant; two: TGVariant): gboolean; cdecl; external;
function g_variant_get_boolean(AVariant: PGVariant): gboolean; cdecl; external;
function g_variant_get_byte(AVariant: PGVariant): guint8; cdecl; external;
function g_variant_get_bytestring(AVariant: PGVariant): Pgchar; cdecl; external;
function g_variant_get_bytestring_array(AVariant: PGVariant; length: Pgsize): PPgchar; cdecl; external;
function g_variant_get_child_value(AVariant: PGVariant; index_: gsize): PGVariant; cdecl; external;
function g_variant_get_data(AVariant: PGVariant): gpointer; cdecl; external;
function g_variant_get_double(AVariant: PGVariant): gdouble; cdecl; external;
function g_variant_get_fixed_array(AVariant: PGVariant; n_elements: Pgsize; element_size: gsize): gpointer; cdecl; external;
function g_variant_get_gtype: TGType; cdecl; external;
function g_variant_get_handle(AVariant: PGVariant): gint32; cdecl; external;
function g_variant_get_int16(AVariant: PGVariant): gint16; cdecl; external;
function g_variant_get_int32(AVariant: PGVariant): gint32; cdecl; external;
function g_variant_get_int64(AVariant: PGVariant): gint64; cdecl; external;
function g_variant_get_maybe(AVariant: PGVariant): PGVariant; cdecl; external;
function g_variant_get_normal_form(AVariant: PGVariant): PGVariant; cdecl; external;
function g_variant_get_objv(AVariant: PGVariant; length: Pgsize): PPgchar; cdecl; external;
function g_variant_get_size(AVariant: PGVariant): gsize; cdecl; external;
function g_variant_get_string(AVariant: PGVariant; length: Pgsize): Pgchar; cdecl; external;
function g_variant_get_strv(AVariant: PGVariant; length: Pgsize): PPgchar; cdecl; external;
function g_variant_get_type(value: PGVariant): PGVariantType; cdecl; external;
function g_variant_get_type_string(AVariant: PGVariant): Pgchar; cdecl; external;
function g_variant_get_uint16(AVariant: PGVariant): guint16; cdecl; external;
function g_variant_get_uint32(AVariant: PGVariant): guint32; cdecl; external;
function g_variant_get_uint64(AVariant: PGVariant): guint64; cdecl; external;
function g_variant_get_variant(AVariant: PGVariant): PGVariant; cdecl; external;
function g_variant_hash(AVariant: PGVariant): guint; cdecl; external;
function g_variant_is_container(AVariant: PGVariant): gboolean; cdecl; external;
function g_variant_is_floating(AVariant: PGVariant): gboolean; cdecl; external;
function g_variant_is_normal_form(AVariant: PGVariant): gboolean; cdecl; external;
function g_variant_is_object_path(string_: Pgchar): gboolean; cdecl; external;
function g_variant_is_of_type(AVariant: PGVariant; type_: PGVariantType): gboolean; cdecl; external;
function g_variant_is_signature(string_: Pgchar): gboolean; cdecl; external;
function g_variant_iter_copy(AVariantIter: PGVariantIter): PGVariantIter; cdecl; external;
function g_variant_iter_init(AVariantIter: PGVariantIter; value: PGVariant): gsize; cdecl; external;
function g_variant_iter_loop(AVariantIter: PGVariantIter; format_string: Pgchar; args: array of const): gboolean; cdecl; external;
function g_variant_iter_n_children(AVariantIter: PGVariantIter): gsize; cdecl; external;
function g_variant_iter_new(AVariant: PGVariant): PGVariantIter; cdecl; external;
function g_variant_iter_next(AVariantIter: PGVariantIter; format_string: Pgchar; args: array of const): gboolean; cdecl; external;
function g_variant_iter_next_value(AVariantIter: PGVariantIter): PGVariant; cdecl; external;
function g_variant_lookup(AVariant: PGVariant; key: Pgchar; format_string: Pgchar; args: array of const): gboolean; cdecl; external;
function g_variant_lookup_value(AVariant: PGVariant; key: Pgchar; expected_type: PGVariantType): PGVariant; cdecl; external;
function g_variant_n_children(AVariant: PGVariant): gsize; cdecl; external;
function g_variant_new(format_string: Pgchar; args: array of const): PGVariant; cdecl; external;
function g_variant_new_array(child_type: PGVariantType; children: PPGVariant; n_children: gsize): PGVariant; cdecl; external;
function g_variant_new_boolean(value: gboolean): PGVariant; cdecl; external;
function g_variant_new_byte(value: guint8): PGVariant; cdecl; external;
function g_variant_new_bytestring(string_: Pgchar): PGVariant; cdecl; external;
function g_variant_new_bytestring_array(strv: PPgchar; length: gssize): PGVariant; cdecl; external;
function g_variant_new_dict_entry(key: PGVariant; value: PGVariant): PGVariant; cdecl; external;
function g_variant_new_double(value: gdouble): PGVariant; cdecl; external;
function g_variant_new_from_data(type_: PGVariantType; data: guint8; size: gsize; trusted: gboolean; notify: TGDestroyNotify; user_data: gpointer): PGVariant; cdecl; external;
function g_variant_new_handle(value: gint32): PGVariant; cdecl; external;
function g_variant_new_int16(value: gint16): PGVariant; cdecl; external;
function g_variant_new_int32(value: gint32): PGVariant; cdecl; external;
function g_variant_new_int64(value: gint64): PGVariant; cdecl; external;
function g_variant_new_maybe(child_type: PGVariantType; child: PGVariant): PGVariant; cdecl; external;
function g_variant_new_object_path(object_path: Pgchar): PGVariant; cdecl; external;
function g_variant_new_objv(strv: PPgchar; length: gssize): PGVariant; cdecl; external;
function g_variant_new_parsed(format: Pgchar; args: array of const): PGVariant; cdecl; external;
function g_variant_new_parsed_va(format: Pgchar; app: Pva_list): PGVariant; cdecl; external;
function g_variant_new_signature(signature: Pgchar): PGVariant; cdecl; external;
function g_variant_new_string(string_: Pgchar): PGVariant; cdecl; external;
function g_variant_new_strv(strv: PPgchar; length: gssize): PGVariant; cdecl; external;
function g_variant_new_tuple(children: PPGVariant; n_children: gsize): PGVariant; cdecl; external;
function g_variant_new_uint16(value: guint16): PGVariant; cdecl; external;
function g_variant_new_uint32(value: guint32): PGVariant; cdecl; external;
function g_variant_new_uint64(value: guint64): PGVariant; cdecl; external;
function g_variant_new_va(format_string: Pgchar; endptr: PPgchar; app: Pva_list): PGVariant; cdecl; external;
function g_variant_new_variant(value: PGVariant): PGVariant; cdecl; external;
function g_variant_parse(type_: PGVariantType; text: Pgchar; limit: Pgchar; endptr: PPgchar): PGVariant; cdecl; external;
function g_variant_parser_get_error_quark: TGQuark; cdecl; external;
function g_variant_print(AVariant: PGVariant; type_annotate: gboolean): Pgchar; cdecl; external;
function g_variant_print_string(AVariant: PGVariant; string_: PGString; type_annotate: gboolean): PGString; cdecl; external;
function g_variant_ref(AVariant: PGVariant): PGVariant; cdecl; external;
function g_variant_ref_sink(AVariant: PGVariant): PGVariant; cdecl; external;
function g_variant_take_ref(AVariant: PGVariant): PGVariant; cdecl; external;
function g_variant_type_checked_(param0: Pgchar): PGVariantType; cdecl; external;
function g_variant_type_copy(AVariantType: PGVariantType): PGVariantType; cdecl; external;
function g_variant_type_dup_string(AVariantType: PGVariantType): Pgchar; cdecl; external;
function g_variant_type_element(AVariantType: PGVariantType): PGVariantType; cdecl; external;
function g_variant_type_equal(AVariantType: PGVariantType; type2: TGVariantType): gboolean; cdecl; external;
function g_variant_type_first(AVariantType: PGVariantType): PGVariantType; cdecl; external;
function g_variant_type_get_gtype: TGType; cdecl; external;
function g_variant_type_get_string_length(AVariantType: PGVariantType): gsize; cdecl; external;
function g_variant_type_hash(AVariantType: PGVariantType): guint; cdecl; external;
function g_variant_type_is_array(AVariantType: PGVariantType): gboolean; cdecl; external;
function g_variant_type_is_basic(AVariantType: PGVariantType): gboolean; cdecl; external;
function g_variant_type_is_container(AVariantType: PGVariantType): gboolean; cdecl; external;
function g_variant_type_is_definite(AVariantType: PGVariantType): gboolean; cdecl; external;
function g_variant_type_is_dict_entry(AVariantType: PGVariantType): gboolean; cdecl; external;
function g_variant_type_is_maybe(AVariantType: PGVariantType): gboolean; cdecl; external;
function g_variant_type_is_subtype_of(AVariantType: PGVariantType; supertype: PGVariantType): gboolean; cdecl; external;
function g_variant_type_is_tuple(AVariantType: PGVariantType): gboolean; cdecl; external;
function g_variant_type_is_variant(AVariantType: PGVariantType): gboolean; cdecl; external;
function g_variant_type_key(AVariantType: PGVariantType): PGVariantType; cdecl; external;
function g_variant_type_n_items(AVariantType: PGVariantType): gsize; cdecl; external;
function g_variant_type_new(type_string: Pgchar): PGVariantType; cdecl; external;
function g_variant_type_new_array(AVariantType: PGVariantType): PGVariantType; cdecl; external;
function g_variant_type_new_dict_entry(AVariantType: PGVariantType; value: PGVariantType): PGVariantType; cdecl; external;
function g_variant_type_new_maybe(AVariantType: PGVariantType): PGVariantType; cdecl; external;
function g_variant_type_new_tuple(items: PPGVariantType; length: gint): PGVariantType; cdecl; external;
function g_variant_type_next(AVariantType: PGVariantType): PGVariantType; cdecl; external;
function g_variant_type_peek_string(AVariantType: PGVariantType): Pgchar; cdecl; external;
function g_variant_type_string_is_valid(type_string: Pgchar): gboolean; cdecl; external;
function g_variant_type_string_scan(string_: Pgchar; limit: Pgchar; endptr: PPgchar): gboolean; cdecl; external;
function g_variant_type_value(AVariantType: PGVariantType): PGVariantType; cdecl; external;
function g_vasprintf(string_: PPgchar; format: Pgchar; args: Tva_list): gint; cdecl; external;
function g_vfprintf(file_: Pgpointer; format: Pgchar; args: Tva_list): gint; cdecl; external;
function g_vprintf(format: Pgchar; args: Tva_list): gint; cdecl; external;
function g_vsnprintf(string_: Pgchar; n: gulong; format: Pgchar; args: Tva_list): gint; cdecl; external;
function g_vsprintf(string_: Pgchar; format: Pgchar; args: Tva_list): gint; cdecl; external;
function glib_check_version(required_major: guint; required_minor: guint; required_micro: guint): Pgchar; cdecl; external;
function intern: TGType; cdecl; external;
procedure g_allocator_free(AAllocator: PGAllocator); cdecl; external;
procedure g_array_sort(array_: Pgpointer; compare_func: TGCompareFunc); cdecl; external;
procedure g_array_sort_with_data(array_: Pgpointer; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_array_unref(array_: Pgpointer); cdecl; external;
procedure g_assert_warning(log_domain: Pgchar; file_: Pgchar; line: gint; pretty_function: Pgchar; expression: Pgchar); cdecl; external;
procedure g_assertion_message(domain: Pgchar; file_: Pgchar; line: gint; func: Pgchar; message: Pgchar); cdecl; external;
procedure g_assertion_message_cmpnum(domain: Pgchar; file_: Pgchar; line: gint; func: Pgchar; expr: Pgchar; arg1: long_double; cmp: Pgchar; arg2: long_double; numtype: char); cdecl; external;
procedure g_assertion_message_cmpstr(domain: Pgchar; file_: Pgchar; line: gint; func: Pgchar; expr: Pgchar; arg1: Pgchar; cmp: Pgchar; arg2: Pgchar); cdecl; external;
procedure g_assertion_message_error(domain: Pgchar; file_: Pgchar; line: gint; func: Pgchar; expr: Pgchar; error: PGError; error_domain: TGQuark; error_code: gint); cdecl; external;
procedure g_assertion_message_expr(domain: Pgchar; file_: Pgchar; line: gint; func: Pgchar; expr: Pgchar); cdecl; external;
procedure g_async_queue_lock(AAsyncQueue: PGAsyncQueue); cdecl; external;
procedure g_async_queue_push(AAsyncQueue: PGAsyncQueue; data: gpointer); cdecl; external;
procedure g_async_queue_push_sorted(AAsyncQueue: PGAsyncQueue; data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_async_queue_push_sorted_unlocked(AAsyncQueue: PGAsyncQueue; data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_async_queue_push_unlocked(AAsyncQueue: PGAsyncQueue; data: gpointer); cdecl; external;
procedure g_async_queue_ref_unlocked(AAsyncQueue: PGAsyncQueue); cdecl; external;
procedure g_async_queue_sort(AAsyncQueue: PGAsyncQueue; func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_async_queue_sort_unlocked(AAsyncQueue: PGAsyncQueue; func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_async_queue_unlock(AAsyncQueue: PGAsyncQueue); cdecl; external;
procedure g_async_queue_unref(AAsyncQueue: PGAsyncQueue); cdecl; external;
procedure g_async_queue_unref_and_unlock(AAsyncQueue: PGAsyncQueue); cdecl; external;
procedure g_atexit(func: TGVoidFunc); cdecl; external;
procedure g_atomic_int_inc(atomic: Pgint); cdecl; external;
procedure g_atomic_int_set(atomic: Pgint; newval: gint); cdecl; external;
procedure g_atomic_pointer_set(atomic: Pgpointer; newval: gpointer); cdecl; external;
procedure g_bit_lock(address: Pgint; lock_bit: gint); cdecl; external;
procedure g_bit_unlock(address: Pgint; lock_bit: gint); cdecl; external;
procedure g_blow_chunks; cdecl; external;
procedure g_bookmark_file_add_application(ABookmarkFile: PGBookmarkFile; uri: Pgchar; name: Pgchar; exec: Pgchar); cdecl; external;
procedure g_bookmark_file_add_group(ABookmarkFile: PGBookmarkFile; uri: Pgchar; group: Pgchar); cdecl; external;
procedure g_bookmark_file_free(ABookmarkFile: PGBookmarkFile); cdecl; external;
procedure g_bookmark_file_set_added(ABookmarkFile: PGBookmarkFile; uri: Pgchar; added: glong); cdecl; external;
procedure g_bookmark_file_set_description(ABookmarkFile: PGBookmarkFile; uri: Pgchar; description: Pgchar); cdecl; external;
procedure g_bookmark_file_set_groups(ABookmarkFile: PGBookmarkFile; uri: Pgchar; groups: PPgchar; length: gsize); cdecl; external;
procedure g_bookmark_file_set_icon(ABookmarkFile: PGBookmarkFile; uri: Pgchar; href: Pgchar; mime_type: Pgchar); cdecl; external;
procedure g_bookmark_file_set_is_private(ABookmarkFile: PGBookmarkFile; uri: Pgchar; is_private: gboolean); cdecl; external;
procedure g_bookmark_file_set_mime_type(ABookmarkFile: PGBookmarkFile; uri: Pgchar; mime_type: Pgchar); cdecl; external;
procedure g_bookmark_file_set_modified(ABookmarkFile: PGBookmarkFile; uri: Pgchar; modified: glong); cdecl; external;
procedure g_bookmark_file_set_title(ABookmarkFile: PGBookmarkFile; uri: Pgchar; title: Pgchar); cdecl; external;
procedure g_bookmark_file_set_visited(ABookmarkFile: PGBookmarkFile; uri: Pgchar; visited: glong); cdecl; external;
procedure g_byte_array_sort(array_: Pguint8; compare_func: TGCompareFunc); cdecl; external;
procedure g_byte_array_sort_with_data(array_: Pguint8; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_byte_array_unref(array_: Pguint8); cdecl; external;
procedure g_cache_destroy(ACache: PGCache); cdecl; external;
procedure g_cache_key_foreach(ACache: PGCache; func: TGHFunc; user_data: gpointer); cdecl; external;
procedure g_cache_remove(ACache: PGCache; value: gpointer); cdecl; external;
procedure g_cache_value_foreach(ACache: PGCache; func: TGHFunc; user_data: gpointer); cdecl; external;
procedure g_checksum_free(AChecksum: PGChecksum); cdecl; external;
procedure g_checksum_get_digest(AChecksum: PGChecksum; buffer: Pguint8; digest_len: Pgsize); cdecl; external;
procedure g_checksum_reset(AChecksum: PGChecksum); cdecl; external;
procedure g_checksum_update(AChecksum: PGChecksum; data: Pguint8; length: gssize); cdecl; external;
procedure g_clear_error; cdecl; external;
procedure g_datalist_clear(datalist: PPGData); cdecl; external;
procedure g_datalist_foreach(datalist: PPGData; func: TGDataForeachFunc; user_data: gpointer); cdecl; external;
procedure g_datalist_id_set_data_full(datalist: PPGData; key_id: TGQuark; data: gpointer; destroy_func: TGDestroyNotify); cdecl; external;
procedure g_datalist_init(datalist: PPGData); cdecl; external;
procedure g_datalist_set_flags(datalist: PPGData; flags: guint); cdecl; external;
procedure g_datalist_unset_flags(datalist: PPGData; flags: guint); cdecl; external;
procedure g_dataset_destroy(dataset_location: gpointer); cdecl; external;
procedure g_dataset_foreach(dataset_location: gpointer; func: TGDataForeachFunc; user_data: gpointer); cdecl; external;
procedure g_dataset_id_set_data_full(dataset_location: gpointer; key_id: TGQuark; data: gpointer; destroy_func: TGDestroyNotify); cdecl; external;
procedure g_date_add_days(ADate: PGDate; n_days: guint); cdecl; external;
procedure g_date_add_months(ADate: PGDate; n_months: guint); cdecl; external;
procedure g_date_add_years(ADate: PGDate; n_years: guint); cdecl; external;
procedure g_date_clamp(ADate: PGDate; min_date: PGDate; max_date: PGDate); cdecl; external;
procedure g_date_clear(ADate: PGDate; n_dates: guint); cdecl; external;
procedure g_date_free(ADate: PGDate); cdecl; external;
procedure g_date_order(ADate: PGDate; date2: PGDate); cdecl; external;
procedure g_date_set_day(ADate: PGDate; day: TGDateDay); cdecl; external;
procedure g_date_set_dmy(ADate: PGDate; day: TGDateDay; month: TGDateMonth; y: TGDateYear); cdecl; external;
procedure g_date_set_julian(ADate: PGDate; julian_date: guint32); cdecl; external;
procedure g_date_set_month(ADate: PGDate; month: TGDateMonth); cdecl; external;
procedure g_date_set_parse(ADate: PGDate; str: Pgchar); cdecl; external;
procedure g_date_set_time_t(ADate: PGDate; timet: glong); cdecl; external;
procedure g_date_set_time_val(ADate: PGDate; timeval: PGTimeVal); cdecl; external;
procedure g_date_set_year(ADate: PGDate; year: TGDateYear); cdecl; external;
procedure g_date_subtract_days(ADate: PGDate; n_days: guint); cdecl; external;
procedure g_date_subtract_months(ADate: PGDate; n_months: guint); cdecl; external;
procedure g_date_subtract_years(ADate: PGDate; n_years: guint); cdecl; external;
procedure g_date_time_get_ymd(ADateTime: PGDateTime; year: Pgint; month: Pgint; day: Pgint); cdecl; external;
procedure g_date_time_unref(ADateTime: PGDateTime); cdecl; external;
procedure g_date_to_struct_tm(ADate: PGDate; tm: Pgpointer); cdecl; external;
procedure g_dir_close(ADir: PGDir); cdecl; external;
procedure g_dir_rewind(ADir: PGDir); cdecl; external;
procedure g_error_free(AError: PGError); cdecl; external;
procedure g_free(mem: gpointer); cdecl; external;
procedure g_get_current_time(result_: PGTimeVal); cdecl; external;
procedure g_hash_table_destroy(hash_table: PGHashTable); cdecl; external;
procedure g_hash_table_foreach(hash_table: PGHashTable; func: TGHFunc; user_data: gpointer); cdecl; external;
procedure g_hash_table_insert(hash_table: PGHashTable; key: gpointer; value: gpointer); cdecl; external;
procedure g_hash_table_iter_init(AHashTableIter: PGHashTableIter; hash_table: PGHashTable); cdecl; external;
procedure g_hash_table_iter_remove(AHashTableIter: PGHashTableIter); cdecl; external;
procedure g_hash_table_iter_replace(AHashTableIter: PGHashTableIter; value: gpointer); cdecl; external;
procedure g_hash_table_iter_steal(AHashTableIter: PGHashTableIter); cdecl; external;
procedure g_hash_table_remove_all(hash_table: PGHashTable); cdecl; external;
procedure g_hash_table_replace(hash_table: PGHashTable; key: gpointer; value: gpointer); cdecl; external;
procedure g_hash_table_steal_all(hash_table: PGHashTable); cdecl; external;
procedure g_hash_table_unref(hash_table: PGHashTable); cdecl; external;
procedure g_hmac_get_digest(AHmac: PGHmac; buffer: Pguint8; digest_len: Pgsize); cdecl; external;
procedure g_hmac_unref(AHmac: PGHmac); cdecl; external;
procedure g_hmac_update(AHmac: PGHmac; data: Pguint8; length: gssize); cdecl; external;
procedure g_hook_destroy_link(hook_list: PGHookList; hook: PGHook); cdecl; external;
procedure g_hook_free(hook_list: PGHookList; hook: PGHook); cdecl; external;
procedure g_hook_insert_before(hook_list: PGHookList; sibling: PGHook; hook: PGHook); cdecl; external;
procedure g_hook_insert_sorted(hook_list: PGHookList; hook: PGHook; func: TGHookCompareFunc); cdecl; external;
procedure g_hook_list_clear(AHookList: PGHookList); cdecl; external;
procedure g_hook_list_init(AHookList: PGHookList; hook_size: guint); cdecl; external;
procedure g_hook_list_invoke(AHookList: PGHookList; may_recurse: gboolean); cdecl; external;
procedure g_hook_list_invoke_check(AHookList: PGHookList; may_recurse: gboolean); cdecl; external;
procedure g_hook_list_marshal(AHookList: PGHookList; may_recurse: gboolean; marshaller: TGHookMarshaller; marshal_data: gpointer); cdecl; external;
procedure g_hook_list_marshal_check(AHookList: PGHookList; may_recurse: gboolean; marshaller: TGHookCheckMarshaller; marshal_data: gpointer); cdecl; external;
procedure g_hook_prepend(hook_list: PGHookList; hook: PGHook); cdecl; external;
procedure g_hook_unref(hook_list: PGHookList; hook: PGHook); cdecl; external;
procedure g_io_channel_close(AIOChannel: PGIOChannel); cdecl; external;
procedure g_io_channel_init(AIOChannel: PGIOChannel); cdecl; external;
procedure g_io_channel_set_buffer_size(AIOChannel: PGIOChannel; size: gsize); cdecl; external;
procedure g_io_channel_set_buffered(AIOChannel: PGIOChannel; buffered: gboolean); cdecl; external;
procedure g_io_channel_set_close_on_unref(AIOChannel: PGIOChannel; do_close: gboolean); cdecl; external;
procedure g_io_channel_set_line_term(AIOChannel: PGIOChannel; line_term: Pgchar; length: gint); cdecl; external;
procedure g_io_channel_unref(AIOChannel: PGIOChannel); cdecl; external;
procedure g_key_file_free(AKeyFile: PGKeyFile); cdecl; external;
procedure g_key_file_set_boolean(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; value: gboolean); cdecl; external;
procedure g_key_file_set_boolean_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; list: gboolean; length: gsize); cdecl; external;
procedure g_key_file_set_double(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; value: gdouble); cdecl; external;
procedure g_key_file_set_double_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; list: gdouble; length: gsize); cdecl; external;
procedure g_key_file_set_int64(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; value: gint64); cdecl; external;
procedure g_key_file_set_integer(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; value: gint); cdecl; external;
procedure g_key_file_set_integer_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; list: gint; length: gsize); cdecl; external;
procedure g_key_file_set_list_separator(AKeyFile: PGKeyFile; separator: gchar); cdecl; external;
procedure g_key_file_set_locale_string(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; locale: Pgchar; string_: Pgchar); cdecl; external;
procedure g_key_file_set_locale_string_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; locale: Pgchar; list: Pgchar; length: gsize); cdecl; external;
procedure g_key_file_set_string(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; string_: Pgchar); cdecl; external;
procedure g_key_file_set_string_list(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; list: Pgchar; length: gsize); cdecl; external;
procedure g_key_file_set_uint64(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; value: guint64); cdecl; external;
procedure g_key_file_set_value(AKeyFile: PGKeyFile; group_name: Pgchar; key: Pgchar; value: Pgchar); cdecl; external;
procedure g_list_foreach(list: PGList; func: TGFunc; user_data: gpointer); cdecl; external;
procedure g_list_free(list: PGList); cdecl; external;
procedure g_list_free_1(list: PGList); cdecl; external;
procedure g_list_free_full(list: PGList; free_func: TGDestroyNotify); cdecl; external;
procedure g_list_pop_allocator; cdecl; external;
procedure g_list_push_allocator(allocator: gpointer); cdecl; external;
procedure g_log(log_domain: Pgchar; log_level: TGLogLevelFlags; format: Pgchar; args: array of const); cdecl; external;
procedure g_log_default_handler(log_domain: Pgchar; log_level: TGLogLevelFlags; message: Pgchar; unused_data: gpointer); cdecl; external;
procedure g_log_remove_handler(log_domain: Pgchar; handler_id: guint); cdecl; external;
procedure g_logv(log_domain: Pgchar; log_level: TGLogLevelFlags; format: Pgchar; args: Tva_list); cdecl; external;
procedure g_main_context_add_poll(AMainContext: PGMainContext; fd: PGPollFD; priority: gint); cdecl; external;
procedure g_main_context_dispatch(AMainContext: PGMainContext); cdecl; external;
procedure g_main_context_invoke(AMainContext: PGMainContext; function_: TGSourceFunc; data: gpointer); cdecl; external;
procedure g_main_context_invoke_full(AMainContext: PGMainContext; priority: gint; function_: TGSourceFunc; data: gpointer; notify: TGDestroyNotify); cdecl; external;
procedure g_main_context_pop_thread_default(AMainContext: PGMainContext); cdecl; external;
procedure g_main_context_push_thread_default(AMainContext: PGMainContext); cdecl; external;
procedure g_main_context_release(AMainContext: PGMainContext); cdecl; external;
procedure g_main_context_remove_poll(AMainContext: PGMainContext; fd: PGPollFD); cdecl; external;
procedure g_main_context_set_poll_func(AMainContext: PGMainContext; func: TGPollFunc); cdecl; external;
procedure g_main_context_unref(AMainContext: PGMainContext); cdecl; external;
procedure g_main_context_wakeup(AMainContext: PGMainContext); cdecl; external;
procedure g_main_loop_quit(AMainLoop: PGMainLoop); cdecl; external;
procedure g_main_loop_run(AMainLoop: PGMainLoop); cdecl; external;
procedure g_main_loop_unref(AMainLoop: PGMainLoop); cdecl; external;
procedure g_mapped_file_free(AMappedFile: PGMappedFile); cdecl; external;
procedure g_mapped_file_unref(AMappedFile: PGMappedFile); cdecl; external;
procedure g_markup_parse_context_free(AMarkupParseContext: PGMarkupParseContext); cdecl; external;
procedure g_markup_parse_context_get_position(AMarkupParseContext: PGMarkupParseContext; line_number: Pgint; char_number: Pgint); cdecl; external;
procedure g_markup_parse_context_push(AMarkupParseContext: PGMarkupParseContext; parser: PGMarkupParser; user_data: gpointer); cdecl; external;
procedure g_match_info_free(AMatchInfo: PGMatchInfo); cdecl; external;
procedure g_match_info_unref(AMatchInfo: PGMatchInfo); cdecl; external;
procedure g_mem_chunk_clean(AMemChunk: PGMemChunk); cdecl; external;
procedure g_mem_chunk_destroy(AMemChunk: PGMemChunk); cdecl; external;
procedure g_mem_chunk_free(AMemChunk: PGMemChunk; mem: gpointer); cdecl; external;
procedure g_mem_chunk_info; cdecl; external;
procedure g_mem_chunk_print(AMemChunk: PGMemChunk); cdecl; external;
procedure g_mem_chunk_reset(AMemChunk: PGMemChunk); cdecl; external;
procedure g_mem_profile; cdecl; external;
procedure g_mem_set_vtable(vtable: PGMemVTable); cdecl; external;
procedure g_node_children_foreach(ANode: PGNode; flags: TGTraverseFlags; func: TGNodeForeachFunc; data: gpointer); cdecl; external;
procedure g_node_destroy(ANode: PGNode); cdecl; external;
procedure g_node_pop_allocator; cdecl; external;
procedure g_node_push_allocator(dummy: gpointer); cdecl; external;
procedure g_node_reverse_children(ANode: PGNode); cdecl; external;
procedure g_node_traverse(ANode: PGNode; order: TGTraverseType; flags: TGTraverseFlags; max_depth: gint; func: TGNodeTraverseFunc; data: gpointer); cdecl; external;
procedure g_node_unlink(ANode: PGNode); cdecl; external;
procedure g_nullify_pointer(nullify_location: Pgpointer); cdecl; external;
procedure g_on_error_query(prg_name: Pgchar); cdecl; external;
procedure g_on_error_stack_trace(prg_name: Pgchar); cdecl; external;
procedure g_once_init_leave(value_location: Pgsize; initialization_value: gsize); cdecl; external;
procedure g_option_context_add_group(AOptionContext: PGOptionContext; group: PGOptionGroup); cdecl; external;
procedure g_option_context_add_main_entries(AOptionContext: PGOptionContext; entries: PGOptionEntry; translation_domain: Pgchar); cdecl; external;
procedure g_option_context_free(AOptionContext: PGOptionContext); cdecl; external;
procedure g_option_context_set_description(AOptionContext: PGOptionContext; description: Pgchar); cdecl; external;
procedure g_option_context_set_help_enabled(AOptionContext: PGOptionContext; help_enabled: gboolean); cdecl; external;
procedure g_option_context_set_ignore_unknown_options(AOptionContext: PGOptionContext; ignore_unknown: gboolean); cdecl; external;
procedure g_option_context_set_main_group(AOptionContext: PGOptionContext; group: PGOptionGroup); cdecl; external;
procedure g_option_context_set_summary(AOptionContext: PGOptionContext; summary: Pgchar); cdecl; external;
procedure g_option_context_set_translate_func(AOptionContext: PGOptionContext; func: TGTranslateFunc; data: gpointer; destroy_notify: TGDestroyNotify); cdecl; external;
procedure g_option_context_set_translation_domain(AOptionContext: PGOptionContext; domain: Pgchar); cdecl; external;
procedure g_option_group_add_entries(AOptionGroup: PGOptionGroup; entries: PGOptionEntry); cdecl; external;
procedure g_option_group_free(AOptionGroup: PGOptionGroup); cdecl; external;
procedure g_option_group_set_error_hook(AOptionGroup: PGOptionGroup; error_func: TGOptionErrorFunc); cdecl; external;
procedure g_option_group_set_parse_hooks(AOptionGroup: PGOptionGroup; pre_parse_func: TGOptionParseFunc; post_parse_func: TGOptionParseFunc); cdecl; external;
procedure g_option_group_set_translate_func(AOptionGroup: PGOptionGroup; func: TGTranslateFunc; data: gpointer; destroy_notify: TGDestroyNotify); cdecl; external;
procedure g_option_group_set_translation_domain(AOptionGroup: PGOptionGroup; domain: Pgchar); cdecl; external;
procedure g_pattern_spec_free(APatternSpec: PGPatternSpec); cdecl; external;
procedure g_pointer_bit_lock(address: Pgpointer; lock_bit: gint); cdecl; external;
procedure g_pointer_bit_unlock(address: Pgpointer; lock_bit: gint); cdecl; external;
procedure g_prefix_error(err: PPGError; format: Pgchar; args: array of const); cdecl; external;
procedure g_print(format: Pgchar; args: array of const); cdecl; external;
procedure g_printerr(format: Pgchar; args: array of const); cdecl; external;
procedure g_propagate_error(dest: PPGError; src: PGError); cdecl; external;
procedure g_propagate_prefixed_error(dest: PPGError; src: PGError; format: Pgchar; args: array of const); cdecl; external;
procedure g_ptr_array_add(array_: Pgpointer; data: gpointer); cdecl; external;
procedure g_ptr_array_foreach(array_: Pgpointer; func: TGFunc; user_data: gpointer); cdecl; external;
procedure g_ptr_array_remove_range(array_: Pgpointer; index_: guint; length: guint); cdecl; external;
procedure g_ptr_array_set_free_func(array_: Pgpointer; element_free_func: TGDestroyNotify); cdecl; external;
procedure g_ptr_array_set_size(array_: Pgpointer; length: gint); cdecl; external;
procedure g_ptr_array_sort(array_: Pgpointer; compare_func: TGCompareFunc); cdecl; external;
procedure g_ptr_array_sort_with_data(array_: Pgpointer; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_ptr_array_unref(array_: Pgpointer); cdecl; external;
procedure g_qsort_with_data(pbase: gpointer; total_elems: gint; size: gsize; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_queue_clear(AQueue: PGQueue); cdecl; external;
procedure g_queue_delete_link(AQueue: PGQueue; link_: PGList); cdecl; external;
procedure g_queue_foreach(AQueue: PGQueue; func: TGFunc; user_data: gpointer); cdecl; external;
procedure g_queue_free(AQueue: PGQueue); cdecl; external;
procedure g_queue_init(AQueue: PGQueue); cdecl; external;
procedure g_queue_insert_after(AQueue: PGQueue; sibling: PGList; data: gpointer); cdecl; external;
procedure g_queue_insert_before(AQueue: PGQueue; sibling: PGList; data: gpointer); cdecl; external;
procedure g_queue_insert_sorted(AQueue: PGQueue; data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_queue_push_head(AQueue: PGQueue; data: gpointer); cdecl; external;
procedure g_queue_push_head_link(AQueue: PGQueue; link_: PGList); cdecl; external;
procedure g_queue_push_nth(AQueue: PGQueue; data: gpointer; n: gint); cdecl; external;
procedure g_queue_push_nth_link(AQueue: PGQueue; n: gint; link_: PGList); cdecl; external;
procedure g_queue_push_tail(AQueue: PGQueue; data: gpointer); cdecl; external;
procedure g_queue_push_tail_link(AQueue: PGQueue; link_: PGList); cdecl; external;
procedure g_queue_reverse(AQueue: PGQueue); cdecl; external;
procedure g_queue_sort(AQueue: PGQueue; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_queue_unlink(AQueue: PGQueue; link_: PGList); cdecl; external;
procedure g_rand_free(ARand: PGRand); cdecl; external;
procedure g_rand_set_seed(ARand: PGRand; seed: guint32); cdecl; external;
procedure g_rand_set_seed_array(ARand: PGRand; seed: Pguint32; seed_length: guint); cdecl; external;
procedure g_random_set_seed(seed: guint32); cdecl; external;
procedure g_regex_unref(ARegex: PGRegex); cdecl; external;
procedure g_reload_user_special_dirs_cache; cdecl; external;
procedure g_return_if_fail_warning(log_domain: Pgchar; pretty_function: Pgchar; expression: Pgchar); cdecl; external;
procedure g_scanner_destroy(AScanner: PGScanner); cdecl; external;
procedure g_scanner_error(AScanner: PGScanner; format: Pgchar; args: array of const); cdecl; external;
procedure g_scanner_input_file(AScanner: PGScanner; input_fd: gint); cdecl; external;
procedure g_scanner_input_text(AScanner: PGScanner; text: Pgchar; text_len: guint); cdecl; external;
procedure g_scanner_scope_add_symbol(AScanner: PGScanner; scope_id: guint; symbol: Pgchar; value: gpointer); cdecl; external;
procedure g_scanner_scope_foreach_symbol(AScanner: PGScanner; scope_id: guint; func: TGHFunc; user_data: gpointer); cdecl; external;
procedure g_scanner_scope_remove_symbol(AScanner: PGScanner; scope_id: guint; symbol: Pgchar); cdecl; external;
procedure g_scanner_sync_file_offset(AScanner: PGScanner); cdecl; external;
procedure g_scanner_unexp_token(AScanner: PGScanner; expected_token: TGTokenType; identifier_spec: Pgchar; symbol_spec: Pgchar; symbol_name: Pgchar; message: Pgchar; is_error: gint); cdecl; external;
procedure g_scanner_warn(AScanner: PGScanner; format: Pgchar; args: array of const); cdecl; external;
procedure g_sequence_foreach(ASequence: PGSequence; func: TGFunc; user_data: gpointer); cdecl; external;
procedure g_sequence_foreach_range(begin_: PGSequenceIter; end_: PGSequenceIter; func: TGFunc; user_data: gpointer); cdecl; external;
procedure g_sequence_free(ASequence: PGSequence); cdecl; external;
procedure g_sequence_move(src: PGSequenceIter; dest: PGSequenceIter); cdecl; external;
procedure g_sequence_move_range(dest: PGSequenceIter; begin_: PGSequenceIter; end_: PGSequenceIter); cdecl; external;
procedure g_sequence_remove(iter: PGSequenceIter); cdecl; external;
procedure g_sequence_remove_range(begin_: PGSequenceIter; end_: PGSequenceIter); cdecl; external;
procedure g_sequence_set(iter: PGSequenceIter; data: gpointer); cdecl; external;
procedure g_sequence_sort(ASequence: PGSequence; cmp_func: TGCompareDataFunc; cmp_data: gpointer); cdecl; external;
procedure g_sequence_sort_changed(iter: PGSequenceIter; cmp_func: TGCompareDataFunc; cmp_data: gpointer); cdecl; external;
procedure g_sequence_sort_changed_iter(iter: PGSequenceIter; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer); cdecl; external;
procedure g_sequence_sort_iter(ASequence: PGSequence; cmp_func: TGSequenceIterCompareFunc; cmp_data: gpointer); cdecl; external;
procedure g_sequence_swap(a: PGSequenceIter; b: PGSequenceIter); cdecl; external;
procedure g_set_application_name(application_name: Pgchar); cdecl; external;
procedure g_set_error(err: PPGError; domain: TGQuark; code: gint; format: Pgchar; args: array of const); cdecl; external;
procedure g_set_error_literal(err: PPGError; domain: TGQuark; code: gint; message: Pgchar); cdecl; external;
procedure g_set_prgname(prgname: Pgchar); cdecl; external;
procedure g_slice_free1(block_size: gsize; mem_block: gpointer); cdecl; external;
procedure g_slice_free_chain_with_offset(block_size: gsize; mem_chain: gpointer; next_offset: gsize); cdecl; external;
procedure g_slice_set_config(ckey: TGSliceConfig; value: gint64); cdecl; external;
procedure g_slist_foreach(list: PGSList; func: TGFunc; user_data: gpointer); cdecl; external;
procedure g_slist_free(list: PGSList); cdecl; external;
procedure g_slist_free_1(list: PGSList); cdecl; external;
procedure g_slist_free_full(list: PGSList; free_func: TGDestroyNotify); cdecl; external;
procedure g_slist_pop_allocator; cdecl; external;
procedure g_slist_push_allocator(dummy: gpointer); cdecl; external;
procedure g_source_add_child_source(ASource: PGSource; child_source: PGSource); cdecl; external;
procedure g_source_add_poll(ASource: PGSource; fd: PGPollFD); cdecl; external;
procedure g_source_destroy(ASource: PGSource); cdecl; external;
procedure g_source_remove_child_source(ASource: PGSource; child_source: PGSource); cdecl; external;
procedure g_source_remove_poll(ASource: PGSource; fd: PGPollFD); cdecl; external;
procedure g_source_set_callback(ASource: PGSource; func: TGSourceFunc; data: gpointer; notify: TGDestroyNotify); cdecl; external;
procedure g_source_set_callback_indirect(ASource: PGSource; callback_data: gpointer; callback_funcs: PGSourceCallbackFuncs); cdecl; external;
procedure g_source_set_can_recurse(ASource: PGSource; can_recurse: gboolean); cdecl; external;
procedure g_source_set_funcs(ASource: PGSource; funcs: PGSourceFuncs); cdecl; external;
procedure g_source_set_name(ASource: PGSource; name: Pgchar); cdecl; external;
procedure g_source_set_name_by_id(tag: guint; name: Pgchar); cdecl; external;
procedure g_source_set_priority(ASource: PGSource; priority: gint); cdecl; external;
procedure g_source_unref(ASource: PGSource); cdecl; external;
procedure g_spawn_close_pid(pid: TGPid); cdecl; external;
procedure g_static_mutex_free(AStaticMutex: PGStaticMutex); cdecl; external;
procedure g_static_mutex_init(AStaticMutex: PGStaticMutex); cdecl; external;
procedure g_static_private_free(AStaticPrivate: PGStaticPrivate); cdecl; external;
procedure g_static_private_init(AStaticPrivate: PGStaticPrivate); cdecl; external;
procedure g_static_private_set(AStaticPrivate: PGStaticPrivate; data: gpointer; notify: TGDestroyNotify); cdecl; external;
procedure g_static_rec_mutex_free(AStaticRecMutex: PGStaticRecMutex); cdecl; external;
procedure g_static_rec_mutex_init(AStaticRecMutex: PGStaticRecMutex); cdecl; external;
procedure g_static_rec_mutex_lock(AStaticRecMutex: PGStaticRecMutex); cdecl; external;
procedure g_static_rec_mutex_lock_full(AStaticRecMutex: PGStaticRecMutex; depth: guint); cdecl; external;
procedure g_static_rec_mutex_unlock(AStaticRecMutex: PGStaticRecMutex); cdecl; external;
procedure g_static_rw_lock_free(AStaticRWLock: PGStaticRWLock); cdecl; external;
procedure g_static_rw_lock_init(AStaticRWLock: PGStaticRWLock); cdecl; external;
procedure g_static_rw_lock_reader_lock(AStaticRWLock: PGStaticRWLock); cdecl; external;
procedure g_static_rw_lock_reader_unlock(AStaticRWLock: PGStaticRWLock); cdecl; external;
procedure g_static_rw_lock_writer_lock(AStaticRWLock: PGStaticRWLock); cdecl; external;
procedure g_static_rw_lock_writer_unlock(AStaticRWLock: PGStaticRWLock); cdecl; external;
procedure g_strfreev(str_array: PPgchar); cdecl; external;
procedure g_string_append_printf(AString: PGString; format: Pgchar; args: array of const); cdecl; external;
procedure g_string_append_vprintf(AString: PGString; format: Pgchar; args: Tva_list); cdecl; external;
procedure g_string_chunk_clear(AStringChunk: PGStringChunk); cdecl; external;
procedure g_string_chunk_free(AStringChunk: PGStringChunk); cdecl; external;
procedure g_string_printf(AString: PGString; format: Pgchar; args: array of const); cdecl; external;
procedure g_string_vprintf(AString: PGString; format: Pgchar; args: Tva_list); cdecl; external;
procedure g_test_add_data_func(testpath: Pgchar; test_data: gpointer; test_func: TGTestDataFunc); cdecl; external;
procedure g_test_add_func(testpath: Pgchar; test_func: TGTestFunc); cdecl; external;
procedure g_test_add_vtable(testpath: Pgchar; data_size: gsize; test_data: gpointer; data_setup: TGTestFixtureFunc; data_test: TGTestFixtureFunc; data_teardown: TGTestFixtureFunc); cdecl; external;
procedure g_test_bug(bug_uri_snippet: Pgchar); cdecl; external;
procedure g_test_bug_base(uri_pattern: Pgchar); cdecl; external;
procedure g_test_fail; cdecl; external;
procedure g_test_init(argc: Pgint; argv: PPPgchar; args: array of const); cdecl; external;
procedure g_test_log_buffer_free(ATestLogBuffer: PGTestLogBuffer); cdecl; external;
procedure g_test_log_buffer_push(ATestLogBuffer: PGTestLogBuffer; n_bytes: guint; bytes: Pguint8); cdecl; external;
procedure g_test_log_msg_free(ATestLogMsg: PGTestLogMsg); cdecl; external;
procedure g_test_log_set_fatal_handler(log_func: TGTestLogFatalFunc; user_data: gpointer); cdecl; external;
procedure g_test_maximized_result(maximized_quantity: gdouble; format: Pgchar; args: array of const); cdecl; external;
procedure g_test_message(format: Pgchar; args: array of const); cdecl; external;
procedure g_test_minimized_result(minimized_quantity: gdouble; format: Pgchar; args: array of const); cdecl; external;
procedure g_test_queue_destroy(destroy_func: TGDestroyNotify; destroy_data: gpointer); cdecl; external;
procedure g_test_queue_free(gfree_pointer: gpointer); cdecl; external;
procedure g_test_suite_add(ATestSuite: PGTestSuite; test_case: PGTestCase); cdecl; external;
procedure g_test_suite_add_suite(ATestSuite: PGTestSuite; nestedsuite: PGTestSuite); cdecl; external;
procedure g_test_timer_start; cdecl; external;
procedure g_test_trap_assertions(domain: Pgchar; file_: Pgchar; line: gint; func: Pgchar; assertion_flags: guint64; pattern: Pgchar); cdecl; external;
procedure g_thread_exit(retval: gpointer); cdecl; external;
procedure g_thread_foreach(thread_func: TGFunc; user_data: gpointer); cdecl; external;
procedure g_thread_init(vtable: PGThreadFunctions); cdecl; external;
procedure g_thread_init_with_errorcheck_mutexes(vtable: PGThreadFunctions); cdecl; external;
procedure g_thread_pool_free(AThreadPool: PGThreadPool; immediate: gboolean; wait_: gboolean); cdecl; external;
procedure g_thread_pool_push(AThreadPool: PGThreadPool; data: gpointer); cdecl; external;
procedure g_thread_pool_set_max_idle_time(interval: guint); cdecl; external;
procedure g_thread_pool_set_max_threads(AThreadPool: PGThreadPool; max_threads: gint); cdecl; external;
procedure g_thread_pool_set_max_unused_threads(max_threads: gint); cdecl; external;
procedure g_thread_pool_set_sort_function(AThreadPool: PGThreadPool; func: TGCompareDataFunc; user_data: gpointer); cdecl; external;
procedure g_thread_pool_stop_unused_threads; cdecl; external;
procedure g_thread_set_priority(AThread: PGThread; priority: TGThreadPriority); cdecl; external;
procedure g_time_val_add(ATimeVal: PGTimeVal; microseconds: glong); cdecl; external;
procedure g_time_zone_unref(ATimeZone: PGTimeZone); cdecl; external;
procedure g_timer_continue(ATimer: PGTimer); cdecl; external;
procedure g_timer_destroy(ATimer: PGTimer); cdecl; external;
procedure g_timer_reset(ATimer: PGTimer); cdecl; external;
procedure g_timer_start(ATimer: PGTimer); cdecl; external;
procedure g_timer_stop(ATimer: PGTimer); cdecl; external;
procedure g_trash_stack_push(stack_p: PPGTrashStack; data_p: gpointer); cdecl; external;
procedure g_tree_destroy(ATree: PGTree); cdecl; external;
procedure g_tree_foreach(ATree: PGTree; func: TGTraverseFunc; user_data: gpointer); cdecl; external;
procedure g_tree_insert(ATree: PGTree; key: gpointer; value: gpointer); cdecl; external;
procedure g_tree_replace(ATree: PGTree; key: gpointer; value: gpointer); cdecl; external;
procedure g_tree_traverse(ATree: PGTree; traverse_func: TGTraverseFunc; traverse_type: TGTraverseType; user_data: gpointer); cdecl; external;
procedure g_tree_unref(ATree: PGTree); cdecl; external;
procedure g_unicode_canonical_ordering(string_: Pgunichar; len: gsize); cdecl; external;
procedure g_unsetenv(variable: Pgchar); cdecl; external;
procedure g_usleep(microseconds: gulong); cdecl; external;
procedure g_variant_builder_add(AVariantBuilder: PGVariantBuilder; format_string: Pgchar; args: array of const); cdecl; external;
procedure g_variant_builder_add_parsed(AVariantBuilder: PGVariantBuilder; format: Pgchar; args: array of const); cdecl; external;
procedure g_variant_builder_add_value(AVariantBuilder: PGVariantBuilder; value: PGVariant); cdecl; external;
procedure g_variant_builder_clear(AVariantBuilder: PGVariantBuilder); cdecl; external;
procedure g_variant_builder_close(AVariantBuilder: PGVariantBuilder); cdecl; external;
procedure g_variant_builder_init(AVariantBuilder: PGVariantBuilder; type_: PGVariantType); cdecl; external;
procedure g_variant_builder_open(AVariantBuilder: PGVariantBuilder; type_: PGVariantType); cdecl; external;
procedure g_variant_builder_unref(AVariantBuilder: PGVariantBuilder); cdecl; external;
procedure g_variant_get(AVariant: PGVariant; format_string: Pgchar; args: array of const); cdecl; external;
procedure g_variant_get_child(AVariant: PGVariant; index_: gsize; format_string: Pgchar; args: array of const); cdecl; external;
procedure g_variant_get_va(AVariant: PGVariant; format_string: Pgchar; endptr: PPgchar; app: Pva_list); cdecl; external;
procedure g_variant_iter_free(AVariantIter: PGVariantIter); cdecl; external;
procedure g_variant_store(AVariant: PGVariant; data: gpointer); cdecl; external;
procedure g_variant_type_free(AVariantType: PGVariantType); cdecl; external;
procedure g_variant_unref(AVariant: PGVariant); cdecl; external;
procedure g_warn_message(domain: Pgchar; file_: Pgchar; line: gint; func: Pgchar; warnexpr: Pgchar); cdecl; external;
procedure glib_dummy_decl; cdecl; external;
implementation
procedure TBitObject32.Init(AFlags: DWord);
begin
  Flags0 := AFlags;
end;

procedure TBitObject32.SetBit(AMask: Integer; AValue: DWord);
begin
  if AValue <> 0 then
  begin
    if (Flags0 and AMask) = 0 then
      Flags0 := Flags0 or AMask
  end
  else begin
    if (Flags0 and AMask) <> 0 then
      Flags0 := Flags0 xor AMask;
  end;
end;

function TBitObject32.GetBit(AMask: Integer): DWord;
begin
  Result := Flags0 and AMask;
  if Result > 1 then
    Result := 1;
end;
procedure TGAllocator.free; cdecl;
begin
  GLib2.g_allocator_free(@self);
end;

function TGAllocator.new(name: Pgchar; n_preallocs: guint): PGAllocator; cdecl;
begin
  Result := GLib2.g_allocator_new(name, n_preallocs);
end;

function TGArray.append_vals(array_: Pgpointer; data: gpointer; len: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_append_vals(array_, data, len);
end;

function TGArray.free(array_: Pgpointer; free_segment: gboolean): Pgchar; cdecl;
begin
  Result := GLib2.g_array_free(array_, free_segment);
end;

function TGArray.get_element_size(array_: Pgpointer): guint; cdecl;
begin
  Result := GLib2.g_array_get_element_size(array_);
end;

function TGArray.insert_vals(array_: Pgpointer; index_: guint; data: gpointer; len: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_insert_vals(array_, index_, data, len);
end;

function TGArray.new(zero_terminated: gboolean; clear_: gboolean; element_size: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_new(zero_terminated, clear_, element_size);
end;

function TGArray.prepend_vals(array_: Pgpointer; data: gpointer; len: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_prepend_vals(array_, data, len);
end;

function TGArray.ref(array_: Pgpointer): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_ref(array_);
end;

function TGArray.remove_index(array_: Pgpointer; index_: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_remove_index(array_, index_);
end;

function TGArray.remove_index_fast(array_: Pgpointer; index_: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_remove_index_fast(array_, index_);
end;

function TGArray.remove_range(array_: Pgpointer; index_: guint; length: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_remove_range(array_, index_, length);
end;

function TGArray.set_size(array_: Pgpointer; length: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_set_size(array_, length);
end;

function TGArray.sized_new(zero_terminated: gboolean; clear_: gboolean; element_size: guint; reserved_size: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_array_sized_new(zero_terminated, clear_, element_size, reserved_size);
end;

procedure TGArray.sort(array_: Pgpointer; compare_func: TGCompareFunc); cdecl;
begin
  GLib2.g_array_sort(array_, compare_func);
end;

procedure TGArray.sort_with_data(array_: Pgpointer; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_array_sort_with_data(array_, compare_func, user_data);
end;

procedure TGArray.unref(array_: Pgpointer); cdecl;
begin
  GLib2.g_array_unref(array_);
end;


function TGAsyncQueue.length: gint; cdecl;
begin
  Result := GLib2.g_async_queue_length(@self);
end;

function TGAsyncQueue.length_unlocked: gint; cdecl;
begin
  Result := GLib2.g_async_queue_length_unlocked(@self);
end;

procedure TGAsyncQueue.lock; cdecl;
begin
  GLib2.g_async_queue_lock(@self);
end;

function TGAsyncQueue.pop: gpointer; cdecl;
begin
  Result := GLib2.g_async_queue_pop(@self);
end;

function TGAsyncQueue.pop_unlocked: gpointer; cdecl;
begin
  Result := GLib2.g_async_queue_pop_unlocked(@self);
end;

procedure TGAsyncQueue.push(data: gpointer); cdecl;
begin
  GLib2.g_async_queue_push(@self, data);
end;

procedure TGAsyncQueue.push_sorted(data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_async_queue_push_sorted(@self, data, func, user_data);
end;

procedure TGAsyncQueue.push_sorted_unlocked(data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_async_queue_push_sorted_unlocked(@self, data, func, user_data);
end;

procedure TGAsyncQueue.push_unlocked(data: gpointer); cdecl;
begin
  GLib2.g_async_queue_push_unlocked(@self, data);
end;

function TGAsyncQueue.ref: PGAsyncQueue; cdecl;
begin
  Result := GLib2.g_async_queue_ref(@self);
end;

procedure TGAsyncQueue.ref_unlocked; cdecl;
begin
  GLib2.g_async_queue_ref_unlocked(@self);
end;

procedure TGAsyncQueue.sort(func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_async_queue_sort(@self, func, user_data);
end;

procedure TGAsyncQueue.sort_unlocked(func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_async_queue_sort_unlocked(@self, func, user_data);
end;

function TGAsyncQueue.timed_pop(end_time: PGTimeVal): gpointer; cdecl;
begin
  Result := GLib2.g_async_queue_timed_pop(@self, end_time);
end;

function TGAsyncQueue.timed_pop_unlocked(end_time: PGTimeVal): gpointer; cdecl;
begin
  Result := GLib2.g_async_queue_timed_pop_unlocked(@self, end_time);
end;

function TGAsyncQueue.try_pop: gpointer; cdecl;
begin
  Result := GLib2.g_async_queue_try_pop(@self);
end;

function TGAsyncQueue.try_pop_unlocked: gpointer; cdecl;
begin
  Result := GLib2.g_async_queue_try_pop_unlocked(@self);
end;

procedure TGAsyncQueue.unlock; cdecl;
begin
  GLib2.g_async_queue_unlock(@self);
end;

procedure TGAsyncQueue.unref; cdecl;
begin
  GLib2.g_async_queue_unref(@self);
end;

procedure TGAsyncQueue.unref_and_unlock; cdecl;
begin
  GLib2.g_async_queue_unref_and_unlock(@self);
end;

function TGAsyncQueue.new: PGAsyncQueue; cdecl;
begin
  Result := GLib2.g_async_queue_new();
end;

function TGAsyncQueue.new_full(item_free_func: TGDestroyNotify): PGAsyncQueue; cdecl;
begin
  Result := GLib2.g_async_queue_new_full(item_free_func);
end;

procedure TGTimeVal.add(microseconds: glong); cdecl;
begin
  GLib2.g_time_val_add(@self, microseconds);
end;

function TGTimeVal.to_iso8601: Pgchar; cdecl;
begin
  Result := GLib2.g_time_val_to_iso8601(@self);
end;

function TGTimeVal.from_iso8601(iso_date: Pgchar; time_: PGTimeVal): gboolean; cdecl;
begin
  Result := GLib2.g_time_val_from_iso8601(iso_date, time_);
end;

procedure TGBookmarkFile.add_application(uri: Pgchar; name: Pgchar; exec: Pgchar); cdecl;
begin
  GLib2.g_bookmark_file_add_application(@self, uri, name, exec);
end;

procedure TGBookmarkFile.add_group(uri: Pgchar; group: Pgchar); cdecl;
begin
  GLib2.g_bookmark_file_add_group(@self, uri, group);
end;

procedure TGBookmarkFile.free; cdecl;
begin
  GLib2.g_bookmark_file_free(@self);
end;

function TGBookmarkFile.get_added(uri: Pgchar): glong; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_added(@self, uri);
end;

function TGBookmarkFile.get_app_info(uri: Pgchar; name: Pgchar; exec: PPgchar; count: Pguint; stamp: Pglong): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_app_info(@self, uri, name, exec, count, stamp);
end;

function TGBookmarkFile.get_applications(uri: Pgchar; length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_applications(@self, uri, length);
end;

function TGBookmarkFile.get_description(uri: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_description(@self, uri);
end;

function TGBookmarkFile.get_groups(uri: Pgchar; length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_groups(@self, uri, length);
end;

function TGBookmarkFile.get_icon(uri: Pgchar; href: PPgchar; mime_type: PPgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_icon(@self, uri, href, mime_type);
end;

function TGBookmarkFile.get_is_private(uri: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_is_private(@self, uri);
end;

function TGBookmarkFile.get_mime_type(uri: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_mime_type(@self, uri);
end;

function TGBookmarkFile.get_modified(uri: Pgchar): glong; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_modified(@self, uri);
end;

function TGBookmarkFile.get_size: gint; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_size(@self);
end;

function TGBookmarkFile.get_title(uri: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_title(@self, uri);
end;

function TGBookmarkFile.get_uris(length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_uris(@self, length);
end;

function TGBookmarkFile.get_visited(uri: Pgchar): glong; cdecl;
begin
  Result := GLib2.g_bookmark_file_get_visited(@self, uri);
end;

function TGBookmarkFile.has_application(uri: Pgchar; name: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_has_application(@self, uri, name);
end;

function TGBookmarkFile.has_group(uri: Pgchar; group: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_has_group(@self, uri, group);
end;

function TGBookmarkFile.has_item(uri: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_has_item(@self, uri);
end;

function TGBookmarkFile.load_from_data(data: Pgchar; length: gsize): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_load_from_data(@self, data, length);
end;

function TGBookmarkFile.load_from_data_dirs(file_: Pgchar; full_path: PPgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_load_from_data_dirs(@self, file_, full_path);
end;

function TGBookmarkFile.load_from_file(filename: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_load_from_file(@self, filename);
end;

function TGBookmarkFile.move_item(old_uri: Pgchar; new_uri: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_move_item(@self, old_uri, new_uri);
end;

function TGBookmarkFile.remove_application(uri: Pgchar; name: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_remove_application(@self, uri, name);
end;

function TGBookmarkFile.remove_group(uri: Pgchar; group: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_remove_group(@self, uri, group);
end;

function TGBookmarkFile.remove_item(uri: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_remove_item(@self, uri);
end;

procedure TGBookmarkFile.set_added(uri: Pgchar; added: glong); cdecl;
begin
  GLib2.g_bookmark_file_set_added(@self, uri, added);
end;

function TGBookmarkFile.set_app_info(uri: Pgchar; name: Pgchar; exec: Pgchar; count: gint; stamp: glong): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_set_app_info(@self, uri, name, exec, count, stamp);
end;

procedure TGBookmarkFile.set_description(uri: Pgchar; description: Pgchar); cdecl;
begin
  GLib2.g_bookmark_file_set_description(@self, uri, description);
end;

procedure TGBookmarkFile.set_groups(uri: Pgchar; groups: PPgchar; length: gsize); cdecl;
begin
  GLib2.g_bookmark_file_set_groups(@self, uri, groups, length);
end;

procedure TGBookmarkFile.set_icon(uri: Pgchar; href: Pgchar; mime_type: Pgchar); cdecl;
begin
  GLib2.g_bookmark_file_set_icon(@self, uri, href, mime_type);
end;

procedure TGBookmarkFile.set_is_private(uri: Pgchar; is_private: gboolean); cdecl;
begin
  GLib2.g_bookmark_file_set_is_private(@self, uri, is_private);
end;

procedure TGBookmarkFile.set_mime_type(uri: Pgchar; mime_type: Pgchar); cdecl;
begin
  GLib2.g_bookmark_file_set_mime_type(@self, uri, mime_type);
end;

procedure TGBookmarkFile.set_modified(uri: Pgchar; modified: glong); cdecl;
begin
  GLib2.g_bookmark_file_set_modified(@self, uri, modified);
end;

procedure TGBookmarkFile.set_title(uri: Pgchar; title: Pgchar); cdecl;
begin
  GLib2.g_bookmark_file_set_title(@self, uri, title);
end;

procedure TGBookmarkFile.set_visited(uri: Pgchar; visited: glong); cdecl;
begin
  GLib2.g_bookmark_file_set_visited(@self, uri, visited);
end;

function TGBookmarkFile.to_data(length: Pgsize): Pgchar; cdecl;
begin
  Result := GLib2.g_bookmark_file_to_data(@self, length);
end;

function TGBookmarkFile.to_file(filename: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_bookmark_file_to_file(@self, filename);
end;

function TGBookmarkFile.error_quark: TGQuark; cdecl;
begin
  Result := GLib2.g_bookmark_file_error_quark();
end;

function TGBookmarkFile.new: PGBookmarkFile; cdecl;
begin
  Result := GLib2.g_bookmark_file_new();
end;

function TGByteArray.append(array_: Pguint8; data: Pguint8; len: guint): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_append(array_, data, len);
end;

function TGByteArray.free(array_: Pguint8; free_segment: gboolean): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_free(array_, free_segment);
end;

function TGByteArray.new: Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_new();
end;

function TGByteArray.prepend(array_: Pguint8; data: Pguint8; len: guint): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_prepend(array_, data, len);
end;

function TGByteArray.ref(array_: Pguint8): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_ref(array_);
end;

function TGByteArray.remove_index(array_: Pguint8; index_: guint): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_remove_index(array_, index_);
end;

function TGByteArray.remove_index_fast(array_: Pguint8; index_: guint): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_remove_index_fast(array_, index_);
end;

function TGByteArray.remove_range(array_: Pguint8; index_: guint; length: guint): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_remove_range(array_, index_, length);
end;

function TGByteArray.set_size(array_: Pguint8; length: guint): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_set_size(array_, length);
end;

function TGByteArray.sized_new(reserved_size: guint): Pguint8; cdecl;
begin
  Result := GLib2.g_byte_array_sized_new(reserved_size);
end;

procedure TGByteArray.sort(array_: Pguint8; compare_func: TGCompareFunc); cdecl;
begin
  GLib2.g_byte_array_sort(array_, compare_func);
end;

procedure TGByteArray.sort_with_data(array_: Pguint8; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_byte_array_sort_with_data(array_, compare_func, user_data);
end;

procedure TGByteArray.unref(array_: Pguint8); cdecl;
begin
  GLib2.g_byte_array_unref(array_);
end;

procedure TGCache.destroy_; cdecl;
begin
  GLib2.g_cache_destroy(@self);
end;

function TGCache.insert(key: gpointer): gpointer; cdecl;
begin
  Result := GLib2.g_cache_insert(@self, key);
end;

procedure TGCache.key_foreach(func: TGHFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_cache_key_foreach(@self, func, user_data);
end;

procedure TGCache.remove(value: gpointer); cdecl;
begin
  GLib2.g_cache_remove(@self, value);
end;

procedure TGCache.value_foreach(func: TGHFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_cache_value_foreach(@self, func, user_data);
end;

function TGCache.new(value_new_func: TGCacheNewFunc; value_destroy_func: TGCacheDestroyFunc; key_dup_func: TGCacheDupFunc; key_destroy_func: TGCacheDestroyFunc; hash_key_func: TGHashFunc; hash_value_func: TGHashFunc; key_equal_func: TGEqualFunc): PGCache; cdecl;
begin
  Result := GLib2.g_cache_new(value_new_func, value_destroy_func, key_dup_func, key_destroy_func, hash_key_func, hash_value_func, key_equal_func);
end;

function TGChecksum.copy: PGChecksum; cdecl;
begin
  Result := GLib2.g_checksum_copy(@self);
end;

procedure TGChecksum.free; cdecl;
begin
  GLib2.g_checksum_free(@self);
end;

procedure TGChecksum.get_digest(buffer: Pguint8; digest_len: Pgsize); cdecl;
begin
  GLib2.g_checksum_get_digest(@self, buffer, digest_len);
end;

function TGChecksum.get_string: Pgchar; cdecl;
begin
  Result := GLib2.g_checksum_get_string(@self);
end;

procedure TGChecksum.reset; cdecl;
begin
  GLib2.g_checksum_reset(@self);
end;

procedure TGChecksum.update(data: Pguint8; length: gssize); cdecl;
begin
  GLib2.g_checksum_update(@self, data, length);
end;

function TGChecksum.new(checksum_type: TGChecksumType): PGChecksum; cdecl;
begin
  Result := GLib2.g_checksum_new(checksum_type);
end;

function TGChecksum.type_get_length(checksum_type: TGChecksumType): gssize; cdecl;
begin
  Result := GLib2.g_checksum_type_get_length(checksum_type);
end;

function TGList.alloc: PGList; cdecl;
begin
  Result := GLib2.g_list_alloc();
end;

function TGList.append(list: PGList; data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_list_append(list, data);
end;

function TGList.concat(list1: PGList; list2: PGList): PGList; cdecl;
begin
  Result := GLib2.g_list_concat(list1, list2);
end;

function TGList.copy(list: PGList): PGList; cdecl;
begin
  Result := GLib2.g_list_copy(list);
end;

function TGList.delete_link(list: PGList; link_: PGList): PGList; cdecl;
begin
  Result := GLib2.g_list_delete_link(list, link_);
end;

function TGList.find(list: PGList; data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_list_find(list, data);
end;

function TGList.find_custom(list: PGList; data: gpointer; func: TGCompareFunc): PGList; cdecl;
begin
  Result := GLib2.g_list_find_custom(list, data, func);
end;

function TGList.first(list: PGList): PGList; cdecl;
begin
  Result := GLib2.g_list_first(list);
end;

procedure TGList.foreach(list: PGList; func: TGFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_list_foreach(list, func, user_data);
end;

procedure TGList.free(list: PGList); cdecl;
begin
  GLib2.g_list_free(list);
end;

procedure TGList.free_1(list: PGList); cdecl;
begin
  GLib2.g_list_free_1(list);
end;

procedure TGList.free_full(list: PGList; free_func: TGDestroyNotify); cdecl;
begin
  GLib2.g_list_free_full(list, free_func);
end;

function TGList.index(list: PGList; data: gpointer): gint; cdecl;
begin
  Result := GLib2.g_list_index(list, data);
end;

function TGList.insert(list: PGList; data: gpointer; position: gint): PGList; cdecl;
begin
  Result := GLib2.g_list_insert(list, data, position);
end;

function TGList.insert_before(list: PGList; sibling: PGList; data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_list_insert_before(list, sibling, data);
end;

function TGList.insert_sorted(list: PGList; data: gpointer; func: TGCompareFunc): PGList; cdecl;
begin
  Result := GLib2.g_list_insert_sorted(list, data, func);
end;

function TGList.insert_sorted_with_data(list: PGList; data: gpointer; func: TGCompareDataFunc; user_data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_list_insert_sorted_with_data(list, data, func, user_data);
end;

function TGList.last(list: PGList): PGList; cdecl;
begin
  Result := GLib2.g_list_last(list);
end;

function TGList.length(list: PGList): guint; cdecl;
begin
  Result := GLib2.g_list_length(list);
end;

function TGList.nth(list: PGList; n: guint): PGList; cdecl;
begin
  Result := GLib2.g_list_nth(list, n);
end;

function TGList.nth_data(list: PGList; n: guint): gpointer; cdecl;
begin
  Result := GLib2.g_list_nth_data(list, n);
end;

function TGList.nth_prev(list: PGList; n: guint): PGList; cdecl;
begin
  Result := GLib2.g_list_nth_prev(list, n);
end;

procedure TGList.pop_allocator; cdecl;
begin
  GLib2.g_list_pop_allocator();
end;

function TGList.position(list: PGList; llink: PGList): gint; cdecl;
begin
  Result := GLib2.g_list_position(list, llink);
end;

function TGList.prepend(list: PGList; data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_list_prepend(list, data);
end;

procedure TGList.push_allocator(allocator: gpointer); cdecl;
begin
  GLib2.g_list_push_allocator(allocator);
end;

function TGList.remove(list: PGList; data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_list_remove(list, data);
end;

function TGList.remove_all(list: PGList; data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_list_remove_all(list, data);
end;

function TGList.remove_link(list: PGList; llink: PGList): PGList; cdecl;
begin
  Result := GLib2.g_list_remove_link(list, llink);
end;

function TGList.reverse(list: PGList): PGList; cdecl;
begin
  Result := GLib2.g_list_reverse(list);
end;

function TGList.sort(list: PGList; compare_func: TGCompareFunc): PGList; cdecl;
begin
  Result := GLib2.g_list_sort(list, compare_func);
end;

function TGList.sort_with_data(list: PGList; compare_func: TGCompareDataFunc; user_data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_list_sort_with_data(list, compare_func, user_data);
end;

function TGCompletion.new(func: TGCompletionFunc): PGCompletion; cdecl;
begin
  Result := GLib2.g_completion_new(func);
end;

function TGDate.new: PGDate; cdecl;
begin
  Result := GLib2.g_date_new();
end;

function TGDate.new_dmy(day: TGDateDay; month: TGDateMonth; year: TGDateYear): PGDate; cdecl;
begin
  Result := GLib2.g_date_new_dmy(day, month, year);
end;

function TGDate.new_julian(julian_day: guint32): PGDate; cdecl;
begin
  Result := GLib2.g_date_new_julian(julian_day);
end;

procedure TGDate.add_days(n_days: guint); cdecl;
begin
  GLib2.g_date_add_days(@self, n_days);
end;

procedure TGDate.add_months(n_months: guint); cdecl;
begin
  GLib2.g_date_add_months(@self, n_months);
end;

procedure TGDate.add_years(n_years: guint); cdecl;
begin
  GLib2.g_date_add_years(@self, n_years);
end;

procedure TGDate.clamp(min_date: PGDate; max_date: PGDate); cdecl;
begin
  GLib2.g_date_clamp(@self, min_date, max_date);
end;

procedure TGDate.clear(n_dates: guint); cdecl;
begin
  GLib2.g_date_clear(@self, n_dates);
end;

function TGDate.compare(rhs: PGDate): gint; cdecl;
begin
  Result := GLib2.g_date_compare(@self, rhs);
end;

function TGDate.days_between(date2: PGDate): gint; cdecl;
begin
  Result := GLib2.g_date_days_between(@self, date2);
end;

procedure TGDate.free; cdecl;
begin
  GLib2.g_date_free(@self);
end;

function TGDate.get_day: TGDateDay; cdecl;
begin
  Result := GLib2.g_date_get_day(@self);
end;

function TGDate.get_day_of_year: guint; cdecl;
begin
  Result := GLib2.g_date_get_day_of_year(@self);
end;

function TGDate.get_iso8601_week_of_year: guint; cdecl;
begin
  Result := GLib2.g_date_get_iso8601_week_of_year(@self);
end;

function TGDate.get_julian: guint32; cdecl;
begin
  Result := GLib2.g_date_get_julian(@self);
end;

function TGDate.get_monday_week_of_year: guint; cdecl;
begin
  Result := GLib2.g_date_get_monday_week_of_year(@self);
end;

function TGDate.get_month: TGDateMonth; cdecl;
begin
  Result := GLib2.g_date_get_month(@self);
end;

function TGDate.get_sunday_week_of_year: guint; cdecl;
begin
  Result := GLib2.g_date_get_sunday_week_of_year(@self);
end;

function TGDate.get_weekday: TGDateWeekday; cdecl;
begin
  Result := GLib2.g_date_get_weekday(@self);
end;

function TGDate.get_year: TGDateYear; cdecl;
begin
  Result := GLib2.g_date_get_year(@self);
end;

function TGDate.is_first_of_month: gboolean; cdecl;
begin
  Result := GLib2.g_date_is_first_of_month(@self);
end;

function TGDate.is_last_of_month: gboolean; cdecl;
begin
  Result := GLib2.g_date_is_last_of_month(@self);
end;

procedure TGDate.order(date2: PGDate); cdecl;
begin
  GLib2.g_date_order(@self, date2);
end;

procedure TGDate.set_day(day: TGDateDay); cdecl;
begin
  GLib2.g_date_set_day(@self, day);
end;

procedure TGDate.set_dmy(day: TGDateDay; month: TGDateMonth; y: TGDateYear); cdecl;
begin
  GLib2.g_date_set_dmy(@self, day, month, y);
end;

procedure TGDate.set_julian(julian_date: guint32); cdecl;
begin
  GLib2.g_date_set_julian(@self, julian_date);
end;

procedure TGDate.set_month(month: TGDateMonth); cdecl;
begin
  GLib2.g_date_set_month(@self, month);
end;

procedure TGDate.set_parse(str: Pgchar); cdecl;
begin
  GLib2.g_date_set_parse(@self, str);
end;

procedure TGDate.set_time_t(timet: glong); cdecl;
begin
  GLib2.g_date_set_time_t(@self, timet);
end;

procedure TGDate.set_time_val(timeval: PGTimeVal); cdecl;
begin
  GLib2.g_date_set_time_val(@self, timeval);
end;

procedure TGDate.set_year(year: TGDateYear); cdecl;
begin
  GLib2.g_date_set_year(@self, year);
end;

procedure TGDate.subtract_days(n_days: guint); cdecl;
begin
  GLib2.g_date_subtract_days(@self, n_days);
end;

procedure TGDate.subtract_months(n_months: guint); cdecl;
begin
  GLib2.g_date_subtract_months(@self, n_months);
end;

procedure TGDate.subtract_years(n_years: guint); cdecl;
begin
  GLib2.g_date_subtract_years(@self, n_years);
end;

procedure TGDate.to_struct_tm(tm: Pgpointer); cdecl;
begin
  GLib2.g_date_to_struct_tm(@self, tm);
end;

function TGDate.valid: gboolean; cdecl;
begin
  Result := GLib2.g_date_valid(@self);
end;

function TGDate.get_days_in_month(month: TGDateMonth; year: TGDateYear): guint8; cdecl;
begin
  Result := GLib2.g_date_get_days_in_month(month, year);
end;

function TGDate.get_monday_weeks_in_year(year: TGDateYear): guint8; cdecl;
begin
  Result := GLib2.g_date_get_monday_weeks_in_year(year);
end;

function TGDate.get_sunday_weeks_in_year(year: TGDateYear): guint8; cdecl;
begin
  Result := GLib2.g_date_get_sunday_weeks_in_year(year);
end;

function TGDate.is_leap_year(year: TGDateYear): gboolean; cdecl;
begin
  Result := GLib2.g_date_is_leap_year(year);
end;

function TGDate.strftime(s: Pgchar; slen: gsize; format: Pgchar; date: PGDate): gsize; cdecl;
begin
  Result := GLib2.g_date_strftime(s, slen, format, date);
end;

function TGDate.valid_day(day: TGDateDay): gboolean; cdecl;
begin
  Result := GLib2.g_date_valid_day(day);
end;

function TGDate.valid_dmy(day: TGDateDay; month: TGDateMonth; year: TGDateYear): gboolean; cdecl;
begin
  Result := GLib2.g_date_valid_dmy(day, month, year);
end;

function TGDate.valid_julian(julian_date: guint32): gboolean; cdecl;
begin
  Result := GLib2.g_date_valid_julian(julian_date);
end;

function TGDate.valid_month(month: TGDateMonth): gboolean; cdecl;
begin
  Result := GLib2.g_date_valid_month(month);
end;

function TGDate.valid_weekday(weekday: TGDateWeekday): gboolean; cdecl;
begin
  Result := GLib2.g_date_valid_weekday(weekday);
end;

function TGDate.valid_year(year: TGDateYear): gboolean; cdecl;
begin
  Result := GLib2.g_date_valid_year(year);
end;

function TGDateTime.new(tz: PGTimeZone; year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new(tz, year, month, day, hour, minute, seconds);
end;

function TGDateTime.new_from_timeval_local(tv: PGTimeVal): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_from_timeval_local(tv);
end;

function TGDateTime.new_from_timeval_utc(tv: PGTimeVal): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_from_timeval_utc(tv);
end;

function TGDateTime.new_from_unix_local(t: gint64): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_from_unix_local(t);
end;

function TGDateTime.new_from_unix_utc(t: gint64): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_from_unix_utc(t);
end;

function TGDateTime.new_local(year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_local(year, month, day, hour, minute, seconds);
end;

function TGDateTime.new_now(tz: PGTimeZone): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_now(tz);
end;

function TGDateTime.new_now_local: PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_now_local();
end;

function TGDateTime.new_now_utc: PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_now_utc();
end;

function TGDateTime.new_utc(year: gint; month: gint; day: gint; hour: gint; minute: gint; seconds: gdouble): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_new_utc(year, month, day, hour, minute, seconds);
end;

function TGDateTime.add(timespan: TGTimeSpan): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add(@self, timespan);
end;

function TGDateTime.add_days(days: gint): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add_days(@self, days);
end;

function TGDateTime.add_full(years: gint; months: gint; days: gint; hours: gint; minutes: gint; seconds: gdouble): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add_full(@self, years, months, days, hours, minutes, seconds);
end;

function TGDateTime.add_hours(hours: gint): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add_hours(@self, hours);
end;

function TGDateTime.add_minutes(minutes: gint): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add_minutes(@self, minutes);
end;

function TGDateTime.add_months(months: gint): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add_months(@self, months);
end;

function TGDateTime.add_seconds(seconds: gdouble): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add_seconds(@self, seconds);
end;

function TGDateTime.add_weeks(weeks: gint): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add_weeks(@self, weeks);
end;

function TGDateTime.add_years(years: gint): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_add_years(@self, years);
end;

function TGDateTime.difference(begin_: PGDateTime): TGTimeSpan; cdecl;
begin
  Result := GLib2.g_date_time_difference(@self, begin_);
end;

function TGDateTime.format(format: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_date_time_format(@self, format);
end;

function TGDateTime.get_day_of_month: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_day_of_month(@self);
end;

function TGDateTime.get_day_of_week: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_day_of_week(@self);
end;

function TGDateTime.get_day_of_year: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_day_of_year(@self);
end;

function TGDateTime.get_hour: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_hour(@self);
end;

function TGDateTime.get_microsecond: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_microsecond(@self);
end;

function TGDateTime.get_minute: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_minute(@self);
end;

function TGDateTime.get_month: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_month(@self);
end;

function TGDateTime.get_second: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_second(@self);
end;

function TGDateTime.get_seconds: gdouble; cdecl;
begin
  Result := GLib2.g_date_time_get_seconds(@self);
end;

function TGDateTime.get_timezone_abbreviation: Pgchar; cdecl;
begin
  Result := GLib2.g_date_time_get_timezone_abbreviation(@self);
end;

function TGDateTime.get_utc_offset: TGTimeSpan; cdecl;
begin
  Result := GLib2.g_date_time_get_utc_offset(@self);
end;

function TGDateTime.get_week_numbering_year: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_week_numbering_year(@self);
end;

function TGDateTime.get_week_of_year: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_week_of_year(@self);
end;

function TGDateTime.get_year: gint; cdecl;
begin
  Result := GLib2.g_date_time_get_year(@self);
end;

procedure TGDateTime.get_ymd(year: Pgint; month: Pgint; day: Pgint); cdecl;
begin
  GLib2.g_date_time_get_ymd(@self, year, month, day);
end;

function TGDateTime.is_daylight_savings: gboolean; cdecl;
begin
  Result := GLib2.g_date_time_is_daylight_savings(@self);
end;

function TGDateTime.ref: PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_ref(@self);
end;

function TGDateTime.source_new(cancel_on_set: gboolean): PGSource; cdecl;
begin
  Result := GLib2.g_date_time_source_new(@self, cancel_on_set);
end;

function TGDateTime.to_local: PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_to_local(@self);
end;

function TGDateTime.to_timeval(tv: PGTimeVal): gboolean; cdecl;
begin
  Result := GLib2.g_date_time_to_timeval(@self, tv);
end;

function TGDateTime.to_timezone(tz: PGTimeZone): PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_to_timezone(@self, tz);
end;

function TGDateTime.to_unix: gint64; cdecl;
begin
  Result := GLib2.g_date_time_to_unix(@self);
end;

function TGDateTime.to_utc: PGDateTime; cdecl;
begin
  Result := GLib2.g_date_time_to_utc(@self);
end;

procedure TGDateTime.unref; cdecl;
begin
  GLib2.g_date_time_unref(@self);
end;

function TGDateTime.compare(dt1: gpointer; dt2: gpointer): gint; cdecl;
begin
  Result := GLib2.g_date_time_compare(dt1, dt2);
end;

function TGDateTime.equal(dt1: gpointer; dt2: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_date_time_equal(dt1, dt2);
end;

function TGDateTime.hash(datetime: gpointer): guint; cdecl;
begin
  Result := GLib2.g_date_time_hash(datetime);
end;

function TGTimeZone.adjust_time(type_: TGTimeType; time_: Pgint64): gint; cdecl;
begin
  Result := GLib2.g_time_zone_adjust_time(@self, type_, time_);
end;

function TGTimeZone.find_interval(type_: TGTimeType; time_: gint64): gint; cdecl;
begin
  Result := GLib2.g_time_zone_find_interval(@self, type_, time_);
end;

function TGTimeZone.get_abbreviation(interval: gint): Pgchar; cdecl;
begin
  Result := GLib2.g_time_zone_get_abbreviation(@self, interval);
end;

function TGTimeZone.get_offset(interval: gint): gint32; cdecl;
begin
  Result := GLib2.g_time_zone_get_offset(@self, interval);
end;

function TGTimeZone.is_dst(interval: gint): gboolean; cdecl;
begin
  Result := GLib2.g_time_zone_is_dst(@self, interval);
end;

function TGTimeZone.ref: PGTimeZone; cdecl;
begin
  Result := GLib2.g_time_zone_ref(@self);
end;

procedure TGTimeZone.unref; cdecl;
begin
  GLib2.g_time_zone_unref(@self);
end;

function TGTimeZone.new(identifier: Pgchar): PGTimeZone; cdecl;
begin
  Result := GLib2.g_time_zone_new(identifier);
end;

function TGTimeZone.new_local: PGTimeZone; cdecl;
begin
  Result := GLib2.g_time_zone_new_local();
end;

function TGTimeZone.new_utc: PGTimeZone; cdecl;
begin
  Result := GLib2.g_time_zone_new_utc();
end;

procedure TGSource.add_child_source(child_source: PGSource); cdecl;
begin
  GLib2.g_source_add_child_source(@self, child_source);
end;

procedure TGSource.add_poll(fd: PGPollFD); cdecl;
begin
  GLib2.g_source_add_poll(@self, fd);
end;

function TGSource.attach(context: PGMainContext): guint; cdecl;
begin
  Result := GLib2.g_source_attach(@self, context);
end;

procedure TGSource.destroy_; cdecl;
begin
  GLib2.g_source_destroy(@self);
end;

function TGSource.get_can_recurse: gboolean; cdecl;
begin
  Result := GLib2.g_source_get_can_recurse(@self);
end;

function TGSource.get_context: PGMainContext; cdecl;
begin
  Result := GLib2.g_source_get_context(@self);
end;

function TGSource.get_id: guint; cdecl;
begin
  Result := GLib2.g_source_get_id(@self);
end;

function TGSource.get_name: Pgchar; cdecl;
begin
  Result := GLib2.g_source_get_name(@self);
end;

function TGSource.get_priority: gint; cdecl;
begin
  Result := GLib2.g_source_get_priority(@self);
end;

function TGSource.get_time: gint64; cdecl;
begin
  Result := GLib2.g_source_get_time(@self);
end;

function TGSource.is_destroyed: gboolean; cdecl;
begin
  Result := GLib2.g_source_is_destroyed(@self);
end;

function TGSource.ref: PGSource; cdecl;
begin
  Result := GLib2.g_source_ref(@self);
end;

procedure TGSource.remove_child_source(child_source: PGSource); cdecl;
begin
  GLib2.g_source_remove_child_source(@self, child_source);
end;

procedure TGSource.remove_poll(fd: PGPollFD); cdecl;
begin
  GLib2.g_source_remove_poll(@self, fd);
end;

procedure TGSource.set_callback(func: TGSourceFunc; data: gpointer; notify: TGDestroyNotify); cdecl;
begin
  GLib2.g_source_set_callback(@self, func, data, notify);
end;

procedure TGSource.set_callback_indirect(callback_data: gpointer; callback_funcs: PGSourceCallbackFuncs); cdecl;
begin
  GLib2.g_source_set_callback_indirect(@self, callback_data, callback_funcs);
end;

procedure TGSource.set_can_recurse(can_recurse: gboolean); cdecl;
begin
  GLib2.g_source_set_can_recurse(@self, can_recurse);
end;

procedure TGSource.set_funcs(funcs: PGSourceFuncs); cdecl;
begin
  GLib2.g_source_set_funcs(@self, funcs);
end;

procedure TGSource.set_name(name: Pgchar); cdecl;
begin
  GLib2.g_source_set_name(@self, name);
end;

procedure TGSource.set_priority(priority: gint); cdecl;
begin
  GLib2.g_source_set_priority(@self, priority);
end;

procedure TGSource.unref; cdecl;
begin
  GLib2.g_source_unref(@self);
end;

function TGSource.new(source_funcs: PGSourceFuncs; struct_size: guint): PGSource; cdecl;
begin
  Result := GLib2.g_source_new(source_funcs, struct_size);
end;

function TGSource.remove(tag: guint): gboolean; cdecl;
begin
  Result := GLib2.g_source_remove(tag);
end;

function TGSource.remove_by_funcs_user_data(funcs: PGSourceFuncs; user_data: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_source_remove_by_funcs_user_data(funcs, user_data);
end;

function TGSource.remove_by_user_data(user_data: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_source_remove_by_user_data(user_data);
end;

procedure TGSource.set_name_by_id(tag: guint; name: Pgchar); cdecl;
begin
  GLib2.g_source_set_name_by_id(tag, name);
end;

procedure TGDir.close; cdecl;
begin
  GLib2.g_dir_close(@self);
end;

function TGDir.read_name: Pgchar; cdecl;
begin
  Result := GLib2.g_dir_read_name(@self);
end;

procedure TGDir.rewind; cdecl;
begin
  GLib2.g_dir_rewind(@self);
end;

function TGDir.make_tmp(tmpl: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_dir_make_tmp(tmpl);
end;

function TGDir.open(path: Pgchar; flags: guint): PGDir; cdecl;
begin
  Result := GLib2.g_dir_open(path, flags);
end;

function TGError.new_literal(domain: TGQuark; code: gint; message: Pgchar): PGError; cdecl;
begin
  Result := GLib2.g_error_new_literal(domain, code, message);
end;

function TGError.copy: PGError; cdecl;
begin
  Result := GLib2.g_error_copy(@self);
end;

procedure TGError.free; cdecl;
begin
  GLib2.g_error_free(@self);
end;

function TGError.matches(domain: TGQuark; code: gint): gboolean; cdecl;
begin
  Result := GLib2.g_error_matches(@self, domain, code);
end;



procedure TGHashTable.destroy_(hash_table: PGHashTable); cdecl;
begin
  GLib2.g_hash_table_destroy(hash_table);
end;

function TGHashTable.find(hash_table: PGHashTable; predicate: TGHRFunc; user_data: gpointer): gpointer; cdecl;
begin
  Result := GLib2.g_hash_table_find(hash_table, predicate, user_data);
end;

procedure TGHashTable.foreach(hash_table: PGHashTable; func: TGHFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_hash_table_foreach(hash_table, func, user_data);
end;

function TGHashTable.foreach_remove(hash_table: PGHashTable; func: TGHRFunc; user_data: gpointer): guint; cdecl;
begin
  Result := GLib2.g_hash_table_foreach_remove(hash_table, func, user_data);
end;

function TGHashTable.foreach_steal(hash_table: PGHashTable; func: TGHRFunc; user_data: gpointer): guint; cdecl;
begin
  Result := GLib2.g_hash_table_foreach_steal(hash_table, func, user_data);
end;

function TGHashTable.get_keys(hash_table: PGHashTable): PGList; cdecl;
begin
  Result := GLib2.g_hash_table_get_keys(hash_table);
end;

function TGHashTable.get_values(hash_table: PGHashTable): PGList; cdecl;
begin
  Result := GLib2.g_hash_table_get_values(hash_table);
end;

procedure TGHashTable.insert(hash_table: PGHashTable; key: gpointer; value: gpointer); cdecl;
begin
  GLib2.g_hash_table_insert(hash_table, key, value);
end;

function TGHashTable.lookup(hash_table: PGHashTable; key: gpointer): gpointer; cdecl;
begin
  Result := GLib2.g_hash_table_lookup(hash_table, key);
end;

function TGHashTable.lookup_extended(hash_table: PGHashTable; lookup_key: gpointer; orig_key: Pgpointer; value: Pgpointer): gboolean; cdecl;
begin
  Result := GLib2.g_hash_table_lookup_extended(hash_table, lookup_key, orig_key, value);
end;

function TGHashTable.new(hash_func: TGHashFunc; key_equal_func: TGEqualFunc): PGHashTable; cdecl;
begin
  Result := GLib2.g_hash_table_new(hash_func, key_equal_func);
end;

function TGHashTable.new_full(hash_func: TGHashFunc; key_equal_func: TGEqualFunc; key_destroy_func: TGDestroyNotify; value_destroy_func: TGDestroyNotify): PGHashTable; cdecl;
begin
  Result := GLib2.g_hash_table_new_full(hash_func, key_equal_func, key_destroy_func, value_destroy_func);
end;

function TGHashTable.ref(hash_table: PGHashTable): PGHashTable; cdecl;
begin
  Result := GLib2.g_hash_table_ref(hash_table);
end;

function TGHashTable.remove(hash_table: PGHashTable; key: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_hash_table_remove(hash_table, key);
end;

procedure TGHashTable.remove_all(hash_table: PGHashTable); cdecl;
begin
  GLib2.g_hash_table_remove_all(hash_table);
end;

procedure TGHashTable.replace(hash_table: PGHashTable; key: gpointer; value: gpointer); cdecl;
begin
  GLib2.g_hash_table_replace(hash_table, key, value);
end;

function TGHashTable.size(hash_table: PGHashTable): guint; cdecl;
begin
  Result := GLib2.g_hash_table_size(hash_table);
end;

function TGHashTable.steal(hash_table: PGHashTable; key: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_hash_table_steal(hash_table, key);
end;

procedure TGHashTable.steal_all(hash_table: PGHashTable); cdecl;
begin
  GLib2.g_hash_table_steal_all(hash_table);
end;

procedure TGHashTable.unref(hash_table: PGHashTable); cdecl;
begin
  GLib2.g_hash_table_unref(hash_table);
end;

function TGHashTableIter.get_hash_table: PGHashTable; cdecl;
begin
  Result := GLib2.g_hash_table_iter_get_hash_table(@self);
end;

procedure TGHashTableIter.init(hash_table: PGHashTable); cdecl;
begin
  GLib2.g_hash_table_iter_init(@self, hash_table);
end;

function TGHashTableIter.next(key: Pgpointer; value: Pgpointer): gboolean; cdecl;
begin
  Result := GLib2.g_hash_table_iter_next(@self, key, value);
end;

procedure TGHashTableIter.remove; cdecl;
begin
  GLib2.g_hash_table_iter_remove(@self);
end;

procedure TGHashTableIter.replace(value: gpointer); cdecl;
begin
  GLib2.g_hash_table_iter_replace(@self, value);
end;

procedure TGHashTableIter.steal; cdecl;
begin
  GLib2.g_hash_table_iter_steal(@self);
end;

function TGHmac.copy: PGHmac; cdecl;
begin
  Result := GLib2.g_hmac_copy(@self);
end;

procedure TGHmac.get_digest(buffer: Pguint8; digest_len: Pgsize); cdecl;
begin
  GLib2.g_hmac_get_digest(@self, buffer, digest_len);
end;

function TGHmac.get_string: Pgchar; cdecl;
begin
  Result := GLib2.g_hmac_get_string(@self);
end;

function TGHmac.ref: PGHmac; cdecl;
begin
  Result := GLib2.g_hmac_ref(@self);
end;

procedure TGHmac.unref; cdecl;
begin
  GLib2.g_hmac_unref(@self);
end;

procedure TGHmac.update(data: Pguint8; length: gssize); cdecl;
begin
  GLib2.g_hmac_update(@self, data, length);
end;

function TGHmac.new(digest_type: TGChecksumType; key: Pguint8; key_len: gsize): PGHmac; cdecl;
begin
  Result := GLib2.g_hmac_new(digest_type, key, key_len);
end;

function TGHook.compare_ids(sibling: PGHook): gint; cdecl;
begin
  Result := GLib2.g_hook_compare_ids(@self, sibling);
end;

function TGHook.alloc(hook_list: PGHookList): PGHook; cdecl;
begin
  Result := GLib2.g_hook_alloc(hook_list);
end;

function TGHook.destroy_(hook_list: PGHookList; hook_id: gulong): gboolean; cdecl;
begin
  Result := GLib2.g_hook_destroy(hook_list, hook_id);
end;

procedure TGHook.destroy_link(hook_list: PGHookList; hook: PGHook); cdecl;
begin
  GLib2.g_hook_destroy_link(hook_list, hook);
end;

function TGHook.find(hook_list: PGHookList; need_valids: gboolean; func: TGHookFindFunc; data: gpointer): PGHook; cdecl;
begin
  Result := GLib2.g_hook_find(hook_list, need_valids, func, data);
end;

function TGHook.find_data(hook_list: PGHookList; need_valids: gboolean; data: gpointer): PGHook; cdecl;
begin
  Result := GLib2.g_hook_find_data(hook_list, need_valids, data);
end;

function TGHook.find_func(hook_list: PGHookList; need_valids: gboolean; func: gpointer): PGHook; cdecl;
begin
  Result := GLib2.g_hook_find_func(hook_list, need_valids, func);
end;

function TGHook.find_func_data(hook_list: PGHookList; need_valids: gboolean; func: gpointer; data: gpointer): PGHook; cdecl;
begin
  Result := GLib2.g_hook_find_func_data(hook_list, need_valids, func, data);
end;

function TGHook.first_valid(hook_list: PGHookList; may_be_in_call: gboolean): PGHook; cdecl;
begin
  Result := GLib2.g_hook_first_valid(hook_list, may_be_in_call);
end;

procedure TGHook.free(hook_list: PGHookList; hook: PGHook); cdecl;
begin
  GLib2.g_hook_free(hook_list, hook);
end;

function TGHook.get(hook_list: PGHookList; hook_id: gulong): PGHook; cdecl;
begin
  Result := GLib2.g_hook_get(hook_list, hook_id);
end;

procedure TGHook.insert_before(hook_list: PGHookList; sibling: PGHook; hook: PGHook); cdecl;
begin
  GLib2.g_hook_insert_before(hook_list, sibling, hook);
end;

procedure TGHook.insert_sorted(hook_list: PGHookList; hook: PGHook; func: TGHookCompareFunc); cdecl;
begin
  GLib2.g_hook_insert_sorted(hook_list, hook, func);
end;

function TGHook.next_valid(hook_list: PGHookList; hook: PGHook; may_be_in_call: gboolean): PGHook; cdecl;
begin
  Result := GLib2.g_hook_next_valid(hook_list, hook, may_be_in_call);
end;

procedure TGHook.prepend(hook_list: PGHookList; hook: PGHook); cdecl;
begin
  GLib2.g_hook_prepend(hook_list, hook);
end;

function TGHook.ref(hook_list: PGHookList; hook: PGHook): PGHook; cdecl;
begin
  Result := GLib2.g_hook_ref(hook_list, hook);
end;

procedure TGHook.unref(hook_list: PGHookList; hook: PGHook); cdecl;
begin
  GLib2.g_hook_unref(hook_list, hook);
end;

procedure TGHookList.clear; cdecl;
begin
  GLib2.g_hook_list_clear(@self);
end;

procedure TGHookList.init(hook_size: guint); cdecl;
begin
  GLib2.g_hook_list_init(@self, hook_size);
end;

procedure TGHookList.invoke(may_recurse: gboolean); cdecl;
begin
  GLib2.g_hook_list_invoke(@self, may_recurse);
end;

procedure TGHookList.invoke_check(may_recurse: gboolean); cdecl;
begin
  GLib2.g_hook_list_invoke_check(@self, may_recurse);
end;

procedure TGHookList.marshal(may_recurse: gboolean; marshaller: TGHookMarshaller; marshal_data: gpointer); cdecl;
begin
  GLib2.g_hook_list_marshal(@self, may_recurse, marshaller, marshal_data);
end;

procedure TGHookList.marshal_check(may_recurse: gboolean; marshaller: TGHookCheckMarshaller; marshal_data: gpointer); cdecl;
begin
  GLib2.g_hook_list_marshal_check(@self, may_recurse, marshaller, marshal_data);
end;


function TGIConv.g_iconv(inbuf: PPgchar; inbytes_left: Pgsize; outbuf: PPgchar; outbytes_left: Pgsize): gsize; cdecl;
begin
  Result := GLib2.g_iconv(@self, inbuf, inbytes_left, outbuf, outbytes_left);
end;

function TGIConv.close: gint; cdecl;
begin
  Result := GLib2.g_iconv_close(@self);
end;

function TGIConv.open(to_codeset: Pgchar; from_codeset: Pgchar): TGIConv; cdecl;
begin
  Result := GLib2.g_iconv_open(to_codeset, from_codeset);
end;



function TGString.append(val: Pgchar): PGString; cdecl;
begin
  Result := GLib2.g_string_append(@self, val);
end;

function TGString.append_c(c: gchar): PGString; cdecl;
begin
  Result := GLib2.g_string_append_c(@self, c);
end;

function TGString.append_len(val: Pgchar; len: gssize): PGString; cdecl;
begin
  Result := GLib2.g_string_append_len(@self, val, len);
end;

function TGString.append_unichar(wc: gunichar): PGString; cdecl;
begin
  Result := GLib2.g_string_append_unichar(@self, wc);
end;

function TGString.append_uri_escaped(unescaped: Pgchar; reserved_chars_allowed: Pgchar; allow_utf8: gboolean): PGString; cdecl;
begin
  Result := GLib2.g_string_append_uri_escaped(@self, unescaped, reserved_chars_allowed, allow_utf8);
end;

function TGString.ascii_down: PGString; cdecl;
begin
  Result := GLib2.g_string_ascii_down(@self);
end;

function TGString.ascii_up: PGString; cdecl;
begin
  Result := GLib2.g_string_ascii_up(@self);
end;

function TGString.assign(rval: Pgchar): PGString; cdecl;
begin
  Result := GLib2.g_string_assign(@self, rval);
end;

function TGString.down: PGString; cdecl;
begin
  Result := GLib2.g_string_down(@self);
end;

function TGString.equal(v2: PGString): gboolean; cdecl;
begin
  Result := GLib2.g_string_equal(@self, v2);
end;

function TGString.erase(pos: gssize; len: gssize): PGString; cdecl;
begin
  Result := GLib2.g_string_erase(@self, pos, len);
end;

function TGString.free(free_segment: gboolean): Pgchar; cdecl;
begin
  Result := GLib2.g_string_free(@self, free_segment);
end;

function TGString.hash: guint; cdecl;
begin
  Result := GLib2.g_string_hash(@self);
end;

function TGString.insert(pos: gssize; val: Pgchar): PGString; cdecl;
begin
  Result := GLib2.g_string_insert(@self, pos, val);
end;

function TGString.insert_c(pos: gssize; c: gchar): PGString; cdecl;
begin
  Result := GLib2.g_string_insert_c(@self, pos, c);
end;

function TGString.insert_len(pos: gssize; val: Pgchar; len: gssize): PGString; cdecl;
begin
  Result := GLib2.g_string_insert_len(@self, pos, val, len);
end;

function TGString.insert_unichar(pos: gssize; wc: gunichar): PGString; cdecl;
begin
  Result := GLib2.g_string_insert_unichar(@self, pos, wc);
end;

function TGString.overwrite(pos: gsize; val: Pgchar): PGString; cdecl;
begin
  Result := GLib2.g_string_overwrite(@self, pos, val);
end;

function TGString.overwrite_len(pos: gsize; val: Pgchar; len: gssize): PGString; cdecl;
begin
  Result := GLib2.g_string_overwrite_len(@self, pos, val, len);
end;

function TGString.prepend(val: Pgchar): PGString; cdecl;
begin
  Result := GLib2.g_string_prepend(@self, val);
end;

function TGString.prepend_c(c: gchar): PGString; cdecl;
begin
  Result := GLib2.g_string_prepend_c(@self, c);
end;

function TGString.prepend_len(val: Pgchar; len: gssize): PGString; cdecl;
begin
  Result := GLib2.g_string_prepend_len(@self, val, len);
end;

function TGString.prepend_unichar(wc: gunichar): PGString; cdecl;
begin
  Result := GLib2.g_string_prepend_unichar(@self, wc);
end;

function TGString.set_size(len: gsize): PGString; cdecl;
begin
  Result := GLib2.g_string_set_size(@self, len);
end;

function TGString.truncate(len: gsize): PGString; cdecl;
begin
  Result := GLib2.g_string_truncate(@self, len);
end;

function TGString.up: PGString; cdecl;
begin
  Result := GLib2.g_string_up(@self);
end;

function TGIOChannel.new_file(filename: Pgchar; mode: Pgchar): PGIOChannel; cdecl;
begin
  Result := GLib2.g_io_channel_new_file(filename, mode);
end;

function TGIOChannel.unix_new(fd: gint): PGIOChannel; cdecl;
begin
  Result := GLib2.g_io_channel_unix_new(fd);
end;

procedure TGIOChannel.close; cdecl;
begin
  GLib2.g_io_channel_close(@self);
end;

function TGIOChannel.flush: TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_flush(@self);
end;

function TGIOChannel.get_buffer_condition: TGIOCondition; cdecl;
begin
  Result := GLib2.g_io_channel_get_buffer_condition(@self);
end;

function TGIOChannel.get_buffer_size: gsize; cdecl;
begin
  Result := GLib2.g_io_channel_get_buffer_size(@self);
end;

function TGIOChannel.get_buffered: gboolean; cdecl;
begin
  Result := GLib2.g_io_channel_get_buffered(@self);
end;

function TGIOChannel.get_close_on_unref: gboolean; cdecl;
begin
  Result := GLib2.g_io_channel_get_close_on_unref(@self);
end;

function TGIOChannel.get_encoding: Pgchar; cdecl;
begin
  Result := GLib2.g_io_channel_get_encoding(@self);
end;

function TGIOChannel.get_flags: TGIOFlags; cdecl;
begin
  Result := GLib2.g_io_channel_get_flags(@self);
end;

function TGIOChannel.get_line_term(length: Pgint): Pgchar; cdecl;
begin
  Result := GLib2.g_io_channel_get_line_term(@self, length);
end;

procedure TGIOChannel.init; cdecl;
begin
  GLib2.g_io_channel_init(@self);
end;

function TGIOChannel.read(buf: Pgchar; count: gsize; bytes_read: Pgsize): TGIOError; cdecl;
begin
  Result := GLib2.g_io_channel_read(@self, buf, count, bytes_read);
end;

function TGIOChannel.read_chars(buf: Pgchar; count: gsize; bytes_read: Pgsize): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_read_chars(@self, buf, count, bytes_read);
end;

function TGIOChannel.read_line(str_return: PPgchar; length: Pgsize; terminator_pos: Pgsize): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_read_line(@self, str_return, length, terminator_pos);
end;

function TGIOChannel.read_line_string(buffer: PGString; terminator_pos: Pgsize): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_read_line_string(@self, buffer, terminator_pos);
end;

function TGIOChannel.read_to_end(str_return: PPgchar; length: Pgsize): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_read_to_end(@self, str_return, length);
end;

function TGIOChannel.read_unichar(thechar: Pgunichar): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_read_unichar(@self, thechar);
end;

function TGIOChannel.ref: PGIOChannel; cdecl;
begin
  Result := GLib2.g_io_channel_ref(@self);
end;

function TGIOChannel.seek(offset: gint64; type_: TGSeekType): TGIOError; cdecl;
begin
  Result := GLib2.g_io_channel_seek(@self, offset, type_);
end;

function TGIOChannel.seek_position(offset: gint64; type_: TGSeekType): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_seek_position(@self, offset, type_);
end;

procedure TGIOChannel.set_buffer_size(size: gsize); cdecl;
begin
  GLib2.g_io_channel_set_buffer_size(@self, size);
end;

procedure TGIOChannel.set_buffered(buffered: gboolean); cdecl;
begin
  GLib2.g_io_channel_set_buffered(@self, buffered);
end;

procedure TGIOChannel.set_close_on_unref(do_close: gboolean); cdecl;
begin
  GLib2.g_io_channel_set_close_on_unref(@self, do_close);
end;

function TGIOChannel.set_encoding(encoding: Pgchar): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_set_encoding(@self, encoding);
end;

function TGIOChannel.set_flags(flags: TGIOFlags): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_set_flags(@self, flags);
end;

procedure TGIOChannel.set_line_term(line_term: Pgchar; length: gint); cdecl;
begin
  GLib2.g_io_channel_set_line_term(@self, line_term, length);
end;

function TGIOChannel.shutdown(flush: gboolean): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_shutdown(@self, flush);
end;

function TGIOChannel.unix_get_fd: gint; cdecl;
begin
  Result := GLib2.g_io_channel_unix_get_fd(@self);
end;

procedure TGIOChannel.unref; cdecl;
begin
  GLib2.g_io_channel_unref(@self);
end;

function TGIOChannel.write(buf: Pgchar; count: gsize; bytes_written: Pgsize): TGIOError; cdecl;
begin
  Result := GLib2.g_io_channel_write(@self, buf, count, bytes_written);
end;

function TGIOChannel.write_chars(buf: Pgchar; count: gssize; bytes_written: Pgsize): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_write_chars(@self, buf, count, bytes_written);
end;

function TGIOChannel.write_unichar(thechar: gunichar): TGIOStatus; cdecl;
begin
  Result := GLib2.g_io_channel_write_unichar(@self, thechar);
end;

function TGIOChannel.error_from_errno(en: gint): TGIOChannelError; cdecl;
begin
  Result := GLib2.g_io_channel_error_from_errno(en);
end;

function TGIOChannel.error_quark: TGQuark; cdecl;
begin
  Result := GLib2.g_io_channel_error_quark();
end;


procedure TGKeyFile.free; cdecl;
begin
  GLib2.g_key_file_free(@self);
end;

function TGKeyFile.get_boolean(group_name: Pgchar; key: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_get_boolean(@self, group_name, key);
end;

function TGKeyFile.get_boolean_list(group_name: Pgchar; key: Pgchar; length: Pgsize): Pgboolean; cdecl;
begin
  Result := GLib2.g_key_file_get_boolean_list(@self, group_name, key, length);
end;

function TGKeyFile.get_comment(group_name: Pgchar; key: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_comment(@self, group_name, key);
end;

function TGKeyFile.get_double(group_name: Pgchar; key: Pgchar): gdouble; cdecl;
begin
  Result := GLib2.g_key_file_get_double(@self, group_name, key);
end;

function TGKeyFile.get_double_list(group_name: Pgchar; key: Pgchar; length: Pgsize): Pgdouble; cdecl;
begin
  Result := GLib2.g_key_file_get_double_list(@self, group_name, key, length);
end;

function TGKeyFile.get_groups(length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_groups(@self, length);
end;

function TGKeyFile.get_int64(group_name: Pgchar; key: Pgchar): gint64; cdecl;
begin
  Result := GLib2.g_key_file_get_int64(@self, group_name, key);
end;

function TGKeyFile.get_integer(group_name: Pgchar; key: Pgchar): gint; cdecl;
begin
  Result := GLib2.g_key_file_get_integer(@self, group_name, key);
end;

function TGKeyFile.get_integer_list(group_name: Pgchar; key: Pgchar; length: Pgsize): Pgint; cdecl;
begin
  Result := GLib2.g_key_file_get_integer_list(@self, group_name, key, length);
end;

function TGKeyFile.get_keys(group_name: Pgchar; length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_keys(@self, group_name, length);
end;

function TGKeyFile.get_locale_string(group_name: Pgchar; key: Pgchar; locale: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_locale_string(@self, group_name, key, locale);
end;

function TGKeyFile.get_locale_string_list(group_name: Pgchar; key: Pgchar; locale: Pgchar; length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_locale_string_list(@self, group_name, key, locale, length);
end;

function TGKeyFile.get_start_group: Pgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_start_group(@self);
end;

function TGKeyFile.get_string(group_name: Pgchar; key: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_string(@self, group_name, key);
end;

function TGKeyFile.get_string_list(group_name: Pgchar; key: Pgchar; length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_string_list(@self, group_name, key, length);
end;

function TGKeyFile.get_uint64(group_name: Pgchar; key: Pgchar): guint64; cdecl;
begin
  Result := GLib2.g_key_file_get_uint64(@self, group_name, key);
end;

function TGKeyFile.get_value(group_name: Pgchar; key: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_key_file_get_value(@self, group_name, key);
end;

function TGKeyFile.has_group(group_name: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_has_group(@self, group_name);
end;

function TGKeyFile.has_key(group_name: Pgchar; key: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_has_key(@self, group_name, key);
end;

function TGKeyFile.load_from_data(data: Pgchar; length: gsize; flags: TGKeyFileFlags): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_load_from_data(@self, data, length, flags);
end;

function TGKeyFile.load_from_data_dirs(file_: Pgchar; full_path: PPgchar; flags: TGKeyFileFlags): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_load_from_data_dirs(@self, file_, full_path, flags);
end;

function TGKeyFile.load_from_dirs(file_: Pgchar; search_dirs: PPgchar; full_path: PPgchar; flags: TGKeyFileFlags): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_load_from_dirs(@self, file_, search_dirs, full_path, flags);
end;

function TGKeyFile.load_from_file(file_: Pgchar; flags: TGKeyFileFlags): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_load_from_file(@self, file_, flags);
end;

function TGKeyFile.remove_comment(group_name: Pgchar; key: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_remove_comment(@self, group_name, key);
end;

function TGKeyFile.remove_group(group_name: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_remove_group(@self, group_name);
end;

function TGKeyFile.remove_key(group_name: Pgchar; key: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_remove_key(@self, group_name, key);
end;

procedure TGKeyFile.set_boolean(group_name: Pgchar; key: Pgchar; value: gboolean); cdecl;
begin
  GLib2.g_key_file_set_boolean(@self, group_name, key, value);
end;

procedure TGKeyFile.set_boolean_list(group_name: Pgchar; key: Pgchar; list: gboolean; length: gsize); cdecl;
begin
  GLib2.g_key_file_set_boolean_list(@self, group_name, key, list, length);
end;

function TGKeyFile.set_comment(group_name: Pgchar; key: Pgchar; comment: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_key_file_set_comment(@self, group_name, key, comment);
end;

procedure TGKeyFile.set_double(group_name: Pgchar; key: Pgchar; value: gdouble); cdecl;
begin
  GLib2.g_key_file_set_double(@self, group_name, key, value);
end;

procedure TGKeyFile.set_double_list(group_name: Pgchar; key: Pgchar; list: gdouble; length: gsize); cdecl;
begin
  GLib2.g_key_file_set_double_list(@self, group_name, key, list, length);
end;

procedure TGKeyFile.set_int64(group_name: Pgchar; key: Pgchar; value: gint64); cdecl;
begin
  GLib2.g_key_file_set_int64(@self, group_name, key, value);
end;

procedure TGKeyFile.set_integer(group_name: Pgchar; key: Pgchar; value: gint); cdecl;
begin
  GLib2.g_key_file_set_integer(@self, group_name, key, value);
end;

procedure TGKeyFile.set_integer_list(group_name: Pgchar; key: Pgchar; list: gint; length: gsize); cdecl;
begin
  GLib2.g_key_file_set_integer_list(@self, group_name, key, list, length);
end;

procedure TGKeyFile.set_list_separator(separator: gchar); cdecl;
begin
  GLib2.g_key_file_set_list_separator(@self, separator);
end;

procedure TGKeyFile.set_locale_string(group_name: Pgchar; key: Pgchar; locale: Pgchar; string_: Pgchar); cdecl;
begin
  GLib2.g_key_file_set_locale_string(@self, group_name, key, locale, string_);
end;

procedure TGKeyFile.set_locale_string_list(group_name: Pgchar; key: Pgchar; locale: Pgchar; list: Pgchar; length: gsize); cdecl;
begin
  GLib2.g_key_file_set_locale_string_list(@self, group_name, key, locale, list, length);
end;

procedure TGKeyFile.set_string(group_name: Pgchar; key: Pgchar; string_: Pgchar); cdecl;
begin
  GLib2.g_key_file_set_string(@self, group_name, key, string_);
end;

procedure TGKeyFile.set_string_list(group_name: Pgchar; key: Pgchar; list: Pgchar; length: gsize); cdecl;
begin
  GLib2.g_key_file_set_string_list(@self, group_name, key, list, length);
end;

procedure TGKeyFile.set_uint64(group_name: Pgchar; key: Pgchar; value: guint64); cdecl;
begin
  GLib2.g_key_file_set_uint64(@self, group_name, key, value);
end;

procedure TGKeyFile.set_value(group_name: Pgchar; key: Pgchar; value: Pgchar); cdecl;
begin
  GLib2.g_key_file_set_value(@self, group_name, key, value);
end;

function TGKeyFile.to_data(length: Pgsize): Pgchar; cdecl;
begin
  Result := GLib2.g_key_file_to_data(@self, length);
end;

function TGKeyFile.error_quark: TGQuark; cdecl;
begin
  Result := GLib2.g_key_file_error_quark();
end;

function TGKeyFile.new: PGKeyFile; cdecl;
begin
  Result := GLib2.g_key_file_new();
end;


function TGMainContext.acquire: gboolean; cdecl;
begin
  Result := GLib2.g_main_context_acquire(@self);
end;

procedure TGMainContext.add_poll(fd: PGPollFD; priority: gint); cdecl;
begin
  GLib2.g_main_context_add_poll(@self, fd, priority);
end;

function TGMainContext.check(max_priority: gint; fds: PGPollFD; n_fds: gint): gint; cdecl;
begin
  Result := GLib2.g_main_context_check(@self, max_priority, fds, n_fds);
end;

procedure TGMainContext.dispatch; cdecl;
begin
  GLib2.g_main_context_dispatch(@self);
end;

function TGMainContext.find_source_by_funcs_user_data(funcs: PGSourceFuncs; user_data: gpointer): PGSource; cdecl;
begin
  Result := GLib2.g_main_context_find_source_by_funcs_user_data(@self, funcs, user_data);
end;

function TGMainContext.find_source_by_id(source_id: guint): PGSource; cdecl;
begin
  Result := GLib2.g_main_context_find_source_by_id(@self, source_id);
end;

function TGMainContext.find_source_by_user_data(user_data: gpointer): PGSource; cdecl;
begin
  Result := GLib2.g_main_context_find_source_by_user_data(@self, user_data);
end;

function TGMainContext.get_poll_func: TGPollFunc; cdecl;
begin
  Result := GLib2.g_main_context_get_poll_func(@self);
end;

procedure TGMainContext.invoke(function_: TGSourceFunc; data: gpointer); cdecl;
begin
  GLib2.g_main_context_invoke(@self, function_, data);
end;

procedure TGMainContext.invoke_full(priority: gint; function_: TGSourceFunc; data: gpointer; notify: TGDestroyNotify); cdecl;
begin
  GLib2.g_main_context_invoke_full(@self, priority, function_, data, notify);
end;

function TGMainContext.is_owner: gboolean; cdecl;
begin
  Result := GLib2.g_main_context_is_owner(@self);
end;

function TGMainContext.iteration(may_block: gboolean): gboolean; cdecl;
begin
  Result := GLib2.g_main_context_iteration(@self, may_block);
end;

function TGMainContext.pending: gboolean; cdecl;
begin
  Result := GLib2.g_main_context_pending(@self);
end;

procedure TGMainContext.pop_thread_default; cdecl;
begin
  GLib2.g_main_context_pop_thread_default(@self);
end;

function TGMainContext.prepare(priority: Pgint): gboolean; cdecl;
begin
  Result := GLib2.g_main_context_prepare(@self, priority);
end;

procedure TGMainContext.push_thread_default; cdecl;
begin
  GLib2.g_main_context_push_thread_default(@self);
end;

function TGMainContext.query(max_priority: gint; timeout_: Pgint; fds: PGPollFD; n_fds: gint): gint; cdecl;
begin
  Result := GLib2.g_main_context_query(@self, max_priority, timeout_, fds, n_fds);
end;

function TGMainContext.ref: PGMainContext; cdecl;
begin
  Result := GLib2.g_main_context_ref(@self);
end;

procedure TGMainContext.release; cdecl;
begin
  GLib2.g_main_context_release(@self);
end;

procedure TGMainContext.remove_poll(fd: PGPollFD); cdecl;
begin
  GLib2.g_main_context_remove_poll(@self, fd);
end;

procedure TGMainContext.set_poll_func(func: TGPollFunc); cdecl;
begin
  GLib2.g_main_context_set_poll_func(@self, func);
end;

procedure TGMainContext.unref; cdecl;
begin
  GLib2.g_main_context_unref(@self);
end;

function TGMainContext.wait(cond: PGCond; mutex: PGMutex): gboolean; cdecl;
begin
  Result := GLib2.g_main_context_wait(@self, cond, mutex);
end;

procedure TGMainContext.wakeup; cdecl;
begin
  GLib2.g_main_context_wakeup(@self);
end;

function TGMainContext.default_: PGMainContext; cdecl;
begin
  Result := GLib2.g_main_context_default();
end;

function TGMainContext.get_thread_default: PGMainContext; cdecl;
begin
  Result := GLib2.g_main_context_get_thread_default();
end;

function TGMainContext.new: PGMainContext; cdecl;
begin
  Result := GLib2.g_main_context_new();
end;

function TGMainLoop.get_context: PGMainContext; cdecl;
begin
  Result := GLib2.g_main_loop_get_context(@self);
end;

function TGMainLoop.is_running: gboolean; cdecl;
begin
  Result := GLib2.g_main_loop_is_running(@self);
end;

procedure TGMainLoop.quit; cdecl;
begin
  GLib2.g_main_loop_quit(@self);
end;

function TGMainLoop.ref: PGMainLoop; cdecl;
begin
  Result := GLib2.g_main_loop_ref(@self);
end;

procedure TGMainLoop.run; cdecl;
begin
  GLib2.g_main_loop_run(@self);
end;

procedure TGMainLoop.unref; cdecl;
begin
  GLib2.g_main_loop_unref(@self);
end;

function TGMainLoop.new(context: PGMainContext; is_running: gboolean): PGMainLoop; cdecl;
begin
  Result := GLib2.g_main_loop_new(context, is_running);
end;

procedure TGMappedFile.free; cdecl;
begin
  GLib2.g_mapped_file_free(@self);
end;

function TGMappedFile.get_contents: Pgchar; cdecl;
begin
  Result := GLib2.g_mapped_file_get_contents(@self);
end;

function TGMappedFile.get_length: gsize; cdecl;
begin
  Result := GLib2.g_mapped_file_get_length(@self);
end;

function TGMappedFile.ref: PGMappedFile; cdecl;
begin
  Result := GLib2.g_mapped_file_ref(@self);
end;

procedure TGMappedFile.unref; cdecl;
begin
  GLib2.g_mapped_file_unref(@self);
end;

function TGMappedFile.new(filename: Pgchar; writable: gboolean): PGMappedFile; cdecl;
begin
  Result := GLib2.g_mapped_file_new(filename, writable);
end;


function TGSList.alloc: PGSList; cdecl;
begin
  Result := GLib2.g_slist_alloc();
end;

function TGSList.append(list: PGSList; data: gpointer): PGSList; cdecl;
begin
  Result := GLib2.g_slist_append(list, data);
end;

function TGSList.concat(list1: PGSList; list2: PGSList): PGSList; cdecl;
begin
  Result := GLib2.g_slist_concat(list1, list2);
end;

function TGSList.copy(list: PGSList): PGSList; cdecl;
begin
  Result := GLib2.g_slist_copy(list);
end;

function TGSList.delete_link(list: PGSList; link_: PGSList): PGSList; cdecl;
begin
  Result := GLib2.g_slist_delete_link(list, link_);
end;

function TGSList.find(list: PGSList; data: gpointer): PGSList; cdecl;
begin
  Result := GLib2.g_slist_find(list, data);
end;

function TGSList.find_custom(list: PGSList; data: gpointer; func: TGCompareFunc): PGSList; cdecl;
begin
  Result := GLib2.g_slist_find_custom(list, data, func);
end;

procedure TGSList.foreach(list: PGSList; func: TGFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_slist_foreach(list, func, user_data);
end;

procedure TGSList.free(list: PGSList); cdecl;
begin
  GLib2.g_slist_free(list);
end;

procedure TGSList.free_1(list: PGSList); cdecl;
begin
  GLib2.g_slist_free_1(list);
end;

procedure TGSList.free_full(list: PGSList; free_func: TGDestroyNotify); cdecl;
begin
  GLib2.g_slist_free_full(list, free_func);
end;

function TGSList.index(list: PGSList; data: gpointer): gint; cdecl;
begin
  Result := GLib2.g_slist_index(list, data);
end;

function TGSList.insert(list: PGSList; data: gpointer; position: gint): PGSList; cdecl;
begin
  Result := GLib2.g_slist_insert(list, data, position);
end;

function TGSList.insert_before(slist: PGSList; sibling: PGSList; data: gpointer): PGSList; cdecl;
begin
  Result := GLib2.g_slist_insert_before(slist, sibling, data);
end;

function TGSList.insert_sorted(list: PGSList; data: gpointer; func: TGCompareFunc): PGSList; cdecl;
begin
  Result := GLib2.g_slist_insert_sorted(list, data, func);
end;

function TGSList.insert_sorted_with_data(list: PGSList; data: gpointer; func: TGCompareDataFunc; user_data: gpointer): PGSList; cdecl;
begin
  Result := GLib2.g_slist_insert_sorted_with_data(list, data, func, user_data);
end;

function TGSList.last(list: PGSList): PGSList; cdecl;
begin
  Result := GLib2.g_slist_last(list);
end;

function TGSList.length(list: PGSList): guint; cdecl;
begin
  Result := GLib2.g_slist_length(list);
end;

function TGSList.nth(list: PGSList; n: guint): PGSList; cdecl;
begin
  Result := GLib2.g_slist_nth(list, n);
end;

function TGSList.nth_data(list: PGSList; n: guint): gpointer; cdecl;
begin
  Result := GLib2.g_slist_nth_data(list, n);
end;

function TGSList.position(list: PGSList; llink: PGSList): gint; cdecl;
begin
  Result := GLib2.g_slist_position(list, llink);
end;

function TGSList.prepend(list: PGSList; data: gpointer): PGSList; cdecl;
begin
  Result := GLib2.g_slist_prepend(list, data);
end;

function TGSList.remove(list: PGSList; data: gpointer): PGSList; cdecl;
begin
  Result := GLib2.g_slist_remove(list, data);
end;

function TGSList.remove_all(list: PGSList; data: gpointer): PGSList; cdecl;
begin
  Result := GLib2.g_slist_remove_all(list, data);
end;

function TGSList.remove_link(list: PGSList; link_: PGSList): PGSList; cdecl;
begin
  Result := GLib2.g_slist_remove_link(list, link_);
end;

function TGSList.reverse(list: PGSList): PGSList; cdecl;
begin
  Result := GLib2.g_slist_reverse(list);
end;

function TGSList.sort(list: PGSList; compare_func: TGCompareFunc): PGSList; cdecl;
begin
  Result := GLib2.g_slist_sort(list, compare_func);
end;

function TGSList.sort_with_data(list: PGSList; compare_func: TGCompareDataFunc; user_data: gpointer): PGSList; cdecl;
begin
  Result := GLib2.g_slist_sort_with_data(list, compare_func, user_data);
end;

function TGMarkupParseContext.end_parse: gboolean; cdecl;
begin
  Result := GLib2.g_markup_parse_context_end_parse(@self);
end;

procedure TGMarkupParseContext.free; cdecl;
begin
  GLib2.g_markup_parse_context_free(@self);
end;

function TGMarkupParseContext.get_element: Pgchar; cdecl;
begin
  Result := GLib2.g_markup_parse_context_get_element(@self);
end;

function TGMarkupParseContext.get_element_stack: PGSList; cdecl;
begin
  Result := GLib2.g_markup_parse_context_get_element_stack(@self);
end;

procedure TGMarkupParseContext.get_position(line_number: Pgint; char_number: Pgint); cdecl;
begin
  GLib2.g_markup_parse_context_get_position(@self, line_number, char_number);
end;

function TGMarkupParseContext.get_user_data: gpointer; cdecl;
begin
  Result := GLib2.g_markup_parse_context_get_user_data(@self);
end;

function TGMarkupParseContext.parse(text: Pgchar; text_len: gssize): gboolean; cdecl;
begin
  Result := GLib2.g_markup_parse_context_parse(@self, text, text_len);
end;

function TGMarkupParseContext.pop: gpointer; cdecl;
begin
  Result := GLib2.g_markup_parse_context_pop(@self);
end;

procedure TGMarkupParseContext.push(parser: PGMarkupParser; user_data: gpointer); cdecl;
begin
  GLib2.g_markup_parse_context_push(@self, parser, user_data);
end;


function TGMarkupParseContext.new(parser: PGMarkupParser; flags: TGMarkupParseFlags; user_data: gpointer; user_data_dnotify: TGDestroyNotify): PGMarkupParseContext; cdecl;
begin
  Result := GLib2.g_markup_parse_context_new(parser, flags, user_data, user_data_dnotify);
end;



function TGRegex.new(pattern: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): PGRegex; cdecl;
begin
  Result := GLib2.g_regex_new(pattern, compile_options, match_options);
end;

function TGRegex.get_capture_count: gint; cdecl;
begin
  Result := GLib2.g_regex_get_capture_count(@self);
end;

function TGRegex.get_compile_flags: TGRegexCompileFlags; cdecl;
begin
  Result := GLib2.g_regex_get_compile_flags(@self);
end;

function TGRegex.get_match_flags: TGRegexMatchFlags; cdecl;
begin
  Result := GLib2.g_regex_get_match_flags(@self);
end;

function TGRegex.get_max_backref: gint; cdecl;
begin
  Result := GLib2.g_regex_get_max_backref(@self);
end;

function TGRegex.get_pattern: Pgchar; cdecl;
begin
  Result := GLib2.g_regex_get_pattern(@self);
end;

function TGRegex.get_string_number(name: Pgchar): gint; cdecl;
begin
  Result := GLib2.g_regex_get_string_number(@self, name);
end;

function TGRegex.match(string_: Pgchar; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl;
begin
  Result := GLib2.g_regex_match(@self, string_, match_options, match_info);
end;

function TGRegex.match_all(string_: Pgchar; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl;
begin
  Result := GLib2.g_regex_match_all(@self, string_, match_options, match_info);
end;

function TGRegex.match_all_full(string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl;
begin
  Result := GLib2.g_regex_match_all_full(@self, string_, string_len, start_position, match_options, match_info);
end;

function TGRegex.match_full(string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; match_info: PPGMatchInfo): gboolean; cdecl;
begin
  Result := GLib2.g_regex_match_full(@self, string_, string_len, start_position, match_options, match_info);
end;

function TGRegex.ref: PGRegex; cdecl;
begin
  Result := GLib2.g_regex_ref(@self);
end;

function TGRegex.replace(string_: Pgchar; string_len: gssize; start_position: gint; replacement: Pgchar; match_options: TGRegexMatchFlags): Pgchar; cdecl;
begin
  Result := GLib2.g_regex_replace(@self, string_, string_len, start_position, replacement, match_options);
end;

function TGRegex.replace_eval(string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; eval: TGRegexEvalCallback; user_data: gpointer): Pgchar; cdecl;
begin
  Result := GLib2.g_regex_replace_eval(@self, string_, string_len, start_position, match_options, eval, user_data);
end;

function TGRegex.replace_literal(string_: Pgchar; string_len: gssize; start_position: gint; replacement: Pgchar; match_options: TGRegexMatchFlags): Pgchar; cdecl;
begin
  Result := GLib2.g_regex_replace_literal(@self, string_, string_len, start_position, replacement, match_options);
end;

function TGRegex.split(string_: Pgchar; match_options: TGRegexMatchFlags): PPgchar; cdecl;
begin
  Result := GLib2.g_regex_split(@self, string_, match_options);
end;

function TGRegex.split_full(string_: Pgchar; string_len: gssize; start_position: gint; match_options: TGRegexMatchFlags; max_tokens: gint): PPgchar; cdecl;
begin
  Result := GLib2.g_regex_split_full(@self, string_, string_len, start_position, match_options, max_tokens);
end;

procedure TGRegex.unref; cdecl;
begin
  GLib2.g_regex_unref(@self);
end;

function TGRegex.check_replacement(replacement: Pgchar; has_references: Pgboolean): gboolean; cdecl;
begin
  Result := GLib2.g_regex_check_replacement(replacement, has_references);
end;

function TGRegex.error_quark: TGQuark; cdecl;
begin
  Result := GLib2.g_regex_error_quark();
end;

function TGRegex.escape_nul(string_: Pgchar; length: gint): Pgchar; cdecl;
begin
  Result := GLib2.g_regex_escape_nul(string_, length);
end;

function TGRegex.escape_string(string_: Pgchar; length: gint): Pgchar; cdecl;
begin
  Result := GLib2.g_regex_escape_string(string_, length);
end;

function TGRegex.match_simple(pattern: Pgchar; string_: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): gboolean; cdecl;
begin
  Result := GLib2.g_regex_match_simple(pattern, string_, compile_options, match_options);
end;

function TGRegex.split_simple(pattern: Pgchar; string_: Pgchar; compile_options: TGRegexCompileFlags; match_options: TGRegexMatchFlags): PPgchar; cdecl;
begin
  Result := GLib2.g_regex_split_simple(pattern, string_, compile_options, match_options);
end;

function TGMatchInfo.expand_references(string_to_expand: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_match_info_expand_references(@self, string_to_expand);
end;

function TGMatchInfo.fetch(match_num: gint): Pgchar; cdecl;
begin
  Result := GLib2.g_match_info_fetch(@self, match_num);
end;

function TGMatchInfo.fetch_all: PPgchar; cdecl;
begin
  Result := GLib2.g_match_info_fetch_all(@self);
end;

function TGMatchInfo.fetch_named(name: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_match_info_fetch_named(@self, name);
end;

function TGMatchInfo.fetch_named_pos(name: Pgchar; start_pos: Pgint; end_pos: Pgint): gboolean; cdecl;
begin
  Result := GLib2.g_match_info_fetch_named_pos(@self, name, start_pos, end_pos);
end;

function TGMatchInfo.fetch_pos(match_num: gint; start_pos: Pgint; end_pos: Pgint): gboolean; cdecl;
begin
  Result := GLib2.g_match_info_fetch_pos(@self, match_num, start_pos, end_pos);
end;

procedure TGMatchInfo.free; cdecl;
begin
  GLib2.g_match_info_free(@self);
end;

function TGMatchInfo.get_match_count: gint; cdecl;
begin
  Result := GLib2.g_match_info_get_match_count(@self);
end;

function TGMatchInfo.get_regex: PGRegex; cdecl;
begin
  Result := GLib2.g_match_info_get_regex(@self);
end;

function TGMatchInfo.get_string: Pgchar; cdecl;
begin
  Result := GLib2.g_match_info_get_string(@self);
end;

function TGMatchInfo.is_partial_match: gboolean; cdecl;
begin
  Result := GLib2.g_match_info_is_partial_match(@self);
end;

function TGMatchInfo.matches: gboolean; cdecl;
begin
  Result := GLib2.g_match_info_matches(@self);
end;

function TGMatchInfo.next: gboolean; cdecl;
begin
  Result := GLib2.g_match_info_next(@self);
end;

function TGMatchInfo.ref: PGMatchInfo; cdecl;
begin
  Result := GLib2.g_match_info_ref(@self);
end;

procedure TGMatchInfo.unref; cdecl;
begin
  GLib2.g_match_info_unref(@self);
end;

function TGMemChunk.alloc: gpointer; cdecl;
begin
  Result := GLib2.g_mem_chunk_alloc(@self);
end;

function TGMemChunk.alloc0: gpointer; cdecl;
begin
  Result := GLib2.g_mem_chunk_alloc0(@self);
end;

procedure TGMemChunk.clean; cdecl;
begin
  GLib2.g_mem_chunk_clean(@self);
end;

procedure TGMemChunk.destroy_; cdecl;
begin
  GLib2.g_mem_chunk_destroy(@self);
end;

procedure TGMemChunk.free(mem: gpointer); cdecl;
begin
  GLib2.g_mem_chunk_free(@self, mem);
end;

procedure TGMemChunk.print; cdecl;
begin
  GLib2.g_mem_chunk_print(@self);
end;

procedure TGMemChunk.reset; cdecl;
begin
  GLib2.g_mem_chunk_reset(@self);
end;

procedure TGMemChunk.info; cdecl;
begin
  GLib2.g_mem_chunk_info();
end;

function TGMemChunk.new(name: Pgchar; atom_size: gint; area_size: gsize; type_: gint): PGMemChunk; cdecl;
begin
  Result := GLib2.g_mem_chunk_new(name, atom_size, area_size, type_);
end;

function TGNode.child_index(data: gpointer): gint; cdecl;
begin
  Result := GLib2.g_node_child_index(@self, data);
end;

function TGNode.child_position(child: PGNode): gint; cdecl;
begin
  Result := GLib2.g_node_child_position(@self, child);
end;


procedure TGNode.children_foreach(flags: TGTraverseFlags; func: TGNodeForeachFunc; data: gpointer); cdecl;
begin
  GLib2.g_node_children_foreach(@self, flags, func, data);
end;

function TGNode.copy: PGNode; cdecl;
begin
  Result := GLib2.g_node_copy(@self);
end;

function TGNode.copy_deep(copy_func: TGCopyFunc; data: gpointer): PGNode; cdecl;
begin
  Result := GLib2.g_node_copy_deep(@self, copy_func, data);
end;

function TGNode.depth: guint; cdecl;
begin
  Result := GLib2.g_node_depth(@self);
end;

procedure TGNode.destroy_; cdecl;
begin
  GLib2.g_node_destroy(@self);
end;

function TGNode.find(order: TGTraverseType; flags: TGTraverseFlags; data: gpointer): PGNode; cdecl;
begin
  Result := GLib2.g_node_find(@self, order, flags, data);
end;

function TGNode.find_child(flags: TGTraverseFlags; data: gpointer): PGNode; cdecl;
begin
  Result := GLib2.g_node_find_child(@self, flags, data);
end;

function TGNode.first_sibling: PGNode; cdecl;
begin
  Result := GLib2.g_node_first_sibling(@self);
end;

function TGNode.get_root: PGNode; cdecl;
begin
  Result := GLib2.g_node_get_root(@self);
end;

function TGNode.insert(position: gint; node: PGNode): PGNode; cdecl;
begin
  Result := GLib2.g_node_insert(@self, position, node);
end;

function TGNode.insert_after(sibling: PGNode; node: PGNode): PGNode; cdecl;
begin
  Result := GLib2.g_node_insert_after(@self, sibling, node);
end;

function TGNode.insert_before(sibling: PGNode; node: PGNode): PGNode; cdecl;
begin
  Result := GLib2.g_node_insert_before(@self, sibling, node);
end;

function TGNode.is_ancestor(descendant: PGNode): gboolean; cdecl;
begin
  Result := GLib2.g_node_is_ancestor(@self, descendant);
end;

function TGNode.last_child: PGNode; cdecl;
begin
  Result := GLib2.g_node_last_child(@self);
end;

function TGNode.last_sibling: PGNode; cdecl;
begin
  Result := GLib2.g_node_last_sibling(@self);
end;

function TGNode.max_height: guint; cdecl;
begin
  Result := GLib2.g_node_max_height(@self);
end;

function TGNode.n_children: guint; cdecl;
begin
  Result := GLib2.g_node_n_children(@self);
end;

function TGNode.n_nodes(flags: TGTraverseFlags): guint; cdecl;
begin
  Result := GLib2.g_node_n_nodes(@self, flags);
end;

function TGNode.nth_child(n: guint): PGNode; cdecl;
begin
  Result := GLib2.g_node_nth_child(@self, n);
end;

function TGNode.prepend(node: PGNode): PGNode; cdecl;
begin
  Result := GLib2.g_node_prepend(@self, node);
end;

procedure TGNode.reverse_children; cdecl;
begin
  GLib2.g_node_reverse_children(@self);
end;

procedure TGNode.traverse(order: TGTraverseType; flags: TGTraverseFlags; max_depth: gint; func: TGNodeTraverseFunc; data: gpointer); cdecl;
begin
  GLib2.g_node_traverse(@self, order, flags, max_depth, func, data);
end;

procedure TGNode.unlink; cdecl;
begin
  GLib2.g_node_unlink(@self);
end;

function TGNode.new(data: gpointer): PGNode; cdecl;
begin
  Result := GLib2.g_node_new(data);
end;

procedure TGNode.pop_allocator; cdecl;
begin
  GLib2.g_node_pop_allocator();
end;

procedure TGNode.push_allocator(dummy: gpointer); cdecl;
begin
  GLib2.g_node_push_allocator(dummy);
end;

function TGOnce.impl(func: TGThreadFunc; arg: gpointer): gpointer; cdecl;
begin
  Result := GLib2.g_once_impl(@self, func, arg);
end;

function TGOnce.init_enter(value_location: Pgsize): gboolean; cdecl;
begin
  Result := GLib2.g_once_init_enter(value_location);
end;

function TGOnce.init_enter_impl(value_location: Pgsize): gboolean; cdecl;
begin
  Result := GLib2.g_once_init_enter_impl(value_location);
end;

procedure TGOnce.init_leave(value_location: Pgsize; initialization_value: gsize); cdecl;
begin
  GLib2.g_once_init_leave(value_location, initialization_value);
end;

procedure TGOptionGroup.add_entries(entries: PGOptionEntry); cdecl;
begin
  GLib2.g_option_group_add_entries(@self, entries);
end;

procedure TGOptionGroup.free; cdecl;
begin
  GLib2.g_option_group_free(@self);
end;

procedure TGOptionGroup.set_error_hook(error_func: TGOptionErrorFunc); cdecl;
begin
  GLib2.g_option_group_set_error_hook(@self, error_func);
end;

procedure TGOptionGroup.set_parse_hooks(pre_parse_func: TGOptionParseFunc; post_parse_func: TGOptionParseFunc); cdecl;
begin
  GLib2.g_option_group_set_parse_hooks(@self, pre_parse_func, post_parse_func);
end;

procedure TGOptionGroup.set_translate_func(func: TGTranslateFunc; data: gpointer; destroy_notify: TGDestroyNotify); cdecl;
begin
  GLib2.g_option_group_set_translate_func(@self, func, data, destroy_notify);
end;

procedure TGOptionGroup.set_translation_domain(domain: Pgchar); cdecl;
begin
  GLib2.g_option_group_set_translation_domain(@self, domain);
end;

function TGOptionGroup.new(name: Pgchar; description: Pgchar; help_description: Pgchar; user_data: gpointer; destroy_: TGDestroyNotify): PGOptionGroup; cdecl;
begin
  Result := GLib2.g_option_group_new(name, description, help_description, user_data, destroy_);
end;

procedure TGOptionContext.add_group(group: PGOptionGroup); cdecl;
begin
  GLib2.g_option_context_add_group(@self, group);
end;

procedure TGOptionContext.add_main_entries(entries: PGOptionEntry; translation_domain: Pgchar); cdecl;
begin
  GLib2.g_option_context_add_main_entries(@self, entries, translation_domain);
end;

procedure TGOptionContext.free; cdecl;
begin
  GLib2.g_option_context_free(@self);
end;

function TGOptionContext.get_description: Pgchar; cdecl;
begin
  Result := GLib2.g_option_context_get_description(@self);
end;

function TGOptionContext.get_help(main_help: gboolean; group: PGOptionGroup): Pgchar; cdecl;
begin
  Result := GLib2.g_option_context_get_help(@self, main_help, group);
end;

function TGOptionContext.get_help_enabled: gboolean; cdecl;
begin
  Result := GLib2.g_option_context_get_help_enabled(@self);
end;

function TGOptionContext.get_ignore_unknown_options: gboolean; cdecl;
begin
  Result := GLib2.g_option_context_get_ignore_unknown_options(@self);
end;

function TGOptionContext.get_main_group: PGOptionGroup; cdecl;
begin
  Result := GLib2.g_option_context_get_main_group(@self);
end;

function TGOptionContext.get_summary: Pgchar; cdecl;
begin
  Result := GLib2.g_option_context_get_summary(@self);
end;

function TGOptionContext.parse(argc: Pgint; argv: PPPgchar): gboolean; cdecl;
begin
  Result := GLib2.g_option_context_parse(@self, argc, argv);
end;

procedure TGOptionContext.set_description(description: Pgchar); cdecl;
begin
  GLib2.g_option_context_set_description(@self, description);
end;

procedure TGOptionContext.set_help_enabled(help_enabled: gboolean); cdecl;
begin
  GLib2.g_option_context_set_help_enabled(@self, help_enabled);
end;

procedure TGOptionContext.set_ignore_unknown_options(ignore_unknown: gboolean); cdecl;
begin
  GLib2.g_option_context_set_ignore_unknown_options(@self, ignore_unknown);
end;

procedure TGOptionContext.set_main_group(group: PGOptionGroup); cdecl;
begin
  GLib2.g_option_context_set_main_group(@self, group);
end;

procedure TGOptionContext.set_summary(summary: Pgchar); cdecl;
begin
  GLib2.g_option_context_set_summary(@self, summary);
end;

procedure TGOptionContext.set_translate_func(func: TGTranslateFunc; data: gpointer; destroy_notify: TGDestroyNotify); cdecl;
begin
  GLib2.g_option_context_set_translate_func(@self, func, data, destroy_notify);
end;

procedure TGOptionContext.set_translation_domain(domain: Pgchar); cdecl;
begin
  GLib2.g_option_context_set_translation_domain(@self, domain);
end;

function TGOptionContext.new(parameter_string: Pgchar): PGOptionContext; cdecl;
begin
  Result := GLib2.g_option_context_new(parameter_string);
end;


function TGPatternSpec.equal(pspec2: PGPatternSpec): gboolean; cdecl;
begin
  Result := GLib2.g_pattern_spec_equal(@self, pspec2);
end;

procedure TGPatternSpec.free; cdecl;
begin
  GLib2.g_pattern_spec_free(@self);
end;

function TGPatternSpec.new(pattern: Pgchar): PGPatternSpec; cdecl;
begin
  Result := GLib2.g_pattern_spec_new(pattern);
end;

procedure TGPtrArray.add(array_: Pgpointer; data: gpointer); cdecl;
begin
  GLib2.g_ptr_array_add(array_, data);
end;

procedure TGPtrArray.foreach(array_: Pgpointer; func: TGFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_ptr_array_foreach(array_, func, user_data);
end;

function TGPtrArray.free(array_: Pgpointer; free_seg: gboolean): Pgpointer; cdecl;
begin
  Result := GLib2.g_ptr_array_free(array_, free_seg);
end;

function TGPtrArray.new: Pgpointer; cdecl;
begin
  Result := GLib2.g_ptr_array_new();
end;

function TGPtrArray.new_full(reserved_size: guint; element_free_func: TGDestroyNotify): Pgpointer; cdecl;
begin
  Result := GLib2.g_ptr_array_new_full(reserved_size, element_free_func);
end;

function TGPtrArray.new_with_free_func(element_free_func: TGDestroyNotify): Pgpointer; cdecl;
begin
  Result := GLib2.g_ptr_array_new_with_free_func(element_free_func);
end;

function TGPtrArray.ref(array_: Pgpointer): Pgpointer; cdecl;
begin
  Result := GLib2.g_ptr_array_ref(array_);
end;

function TGPtrArray.remove(array_: Pgpointer; data: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_ptr_array_remove(array_, data);
end;

function TGPtrArray.remove_fast(array_: Pgpointer; data: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_ptr_array_remove_fast(array_, data);
end;

function TGPtrArray.remove_index(array_: Pgpointer; index_: guint): gpointer; cdecl;
begin
  Result := GLib2.g_ptr_array_remove_index(array_, index_);
end;

function TGPtrArray.remove_index_fast(array_: Pgpointer; index_: guint): gpointer; cdecl;
begin
  Result := GLib2.g_ptr_array_remove_index_fast(array_, index_);
end;

procedure TGPtrArray.remove_range(array_: Pgpointer; index_: guint; length: guint); cdecl;
begin
  GLib2.g_ptr_array_remove_range(array_, index_, length);
end;

procedure TGPtrArray.set_free_func(array_: Pgpointer; element_free_func: TGDestroyNotify); cdecl;
begin
  GLib2.g_ptr_array_set_free_func(array_, element_free_func);
end;

procedure TGPtrArray.set_size(array_: Pgpointer; length: gint); cdecl;
begin
  GLib2.g_ptr_array_set_size(array_, length);
end;

function TGPtrArray.sized_new(reserved_size: guint): Pgpointer; cdecl;
begin
  Result := GLib2.g_ptr_array_sized_new(reserved_size);
end;

procedure TGPtrArray.sort(array_: Pgpointer; compare_func: TGCompareFunc); cdecl;
begin
  GLib2.g_ptr_array_sort(array_, compare_func);
end;

procedure TGPtrArray.sort_with_data(array_: Pgpointer; compare_func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_ptr_array_sort_with_data(array_, compare_func, user_data);
end;

procedure TGPtrArray.unref(array_: Pgpointer); cdecl;
begin
  GLib2.g_ptr_array_unref(array_);
end;

procedure TGQueue.clear; cdecl;
begin
  GLib2.g_queue_clear(@self);
end;

function TGQueue.copy: PGQueue; cdecl;
begin
  Result := GLib2.g_queue_copy(@self);
end;

procedure TGQueue.delete_link(link_: PGList); cdecl;
begin
  GLib2.g_queue_delete_link(@self, link_);
end;

function TGQueue.find(data: gpointer): PGList; cdecl;
begin
  Result := GLib2.g_queue_find(@self, data);
end;

function TGQueue.find_custom(data: gpointer; func: TGCompareFunc): PGList; cdecl;
begin
  Result := GLib2.g_queue_find_custom(@self, data, func);
end;

procedure TGQueue.foreach(func: TGFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_queue_foreach(@self, func, user_data);
end;

procedure TGQueue.free; cdecl;
begin
  GLib2.g_queue_free(@self);
end;

function TGQueue.get_length: guint; cdecl;
begin
  Result := GLib2.g_queue_get_length(@self);
end;

function TGQueue.index(data: gpointer): gint; cdecl;
begin
  Result := GLib2.g_queue_index(@self, data);
end;

procedure TGQueue.init; cdecl;
begin
  GLib2.g_queue_init(@self);
end;

procedure TGQueue.insert_after(sibling: PGList; data: gpointer); cdecl;
begin
  GLib2.g_queue_insert_after(@self, sibling, data);
end;

procedure TGQueue.insert_before(sibling: PGList; data: gpointer); cdecl;
begin
  GLib2.g_queue_insert_before(@self, sibling, data);
end;

procedure TGQueue.insert_sorted(data: gpointer; func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_queue_insert_sorted(@self, data, func, user_data);
end;

function TGQueue.is_empty: gboolean; cdecl;
begin
  Result := GLib2.g_queue_is_empty(@self);
end;

function TGQueue.link_index(link_: PGList): gint; cdecl;
begin
  Result := GLib2.g_queue_link_index(@self, link_);
end;

function TGQueue.peek_head: gpointer; cdecl;
begin
  Result := GLib2.g_queue_peek_head(@self);
end;

function TGQueue.peek_head_link: PGList; cdecl;
begin
  Result := GLib2.g_queue_peek_head_link(@self);
end;

function TGQueue.peek_nth(n: guint): gpointer; cdecl;
begin
  Result := GLib2.g_queue_peek_nth(@self, n);
end;

function TGQueue.peek_nth_link(n: guint): PGList; cdecl;
begin
  Result := GLib2.g_queue_peek_nth_link(@self, n);
end;

function TGQueue.peek_tail: gpointer; cdecl;
begin
  Result := GLib2.g_queue_peek_tail(@self);
end;

function TGQueue.peek_tail_link: PGList; cdecl;
begin
  Result := GLib2.g_queue_peek_tail_link(@self);
end;

function TGQueue.pop_head: gpointer; cdecl;
begin
  Result := GLib2.g_queue_pop_head(@self);
end;

function TGQueue.pop_head_link: PGList; cdecl;
begin
  Result := GLib2.g_queue_pop_head_link(@self);
end;

function TGQueue.pop_nth(n: guint): gpointer; cdecl;
begin
  Result := GLib2.g_queue_pop_nth(@self, n);
end;

function TGQueue.pop_nth_link(n: guint): PGList; cdecl;
begin
  Result := GLib2.g_queue_pop_nth_link(@self, n);
end;

function TGQueue.pop_tail: gpointer; cdecl;
begin
  Result := GLib2.g_queue_pop_tail(@self);
end;

function TGQueue.pop_tail_link: PGList; cdecl;
begin
  Result := GLib2.g_queue_pop_tail_link(@self);
end;

procedure TGQueue.push_head(data: gpointer); cdecl;
begin
  GLib2.g_queue_push_head(@self, data);
end;

procedure TGQueue.push_head_link(link_: PGList); cdecl;
begin
  GLib2.g_queue_push_head_link(@self, link_);
end;

procedure TGQueue.push_nth(data: gpointer; n: gint); cdecl;
begin
  GLib2.g_queue_push_nth(@self, data, n);
end;

procedure TGQueue.push_nth_link(n: gint; link_: PGList); cdecl;
begin
  GLib2.g_queue_push_nth_link(@self, n, link_);
end;

procedure TGQueue.push_tail(data: gpointer); cdecl;
begin
  GLib2.g_queue_push_tail(@self, data);
end;

procedure TGQueue.push_tail_link(link_: PGList); cdecl;
begin
  GLib2.g_queue_push_tail_link(@self, link_);
end;

function TGQueue.remove(data: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_queue_remove(@self, data);
end;

function TGQueue.remove_all(data: gpointer): guint; cdecl;
begin
  Result := GLib2.g_queue_remove_all(@self, data);
end;

procedure TGQueue.reverse; cdecl;
begin
  GLib2.g_queue_reverse(@self);
end;

procedure TGQueue.sort(compare_func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_queue_sort(@self, compare_func, user_data);
end;

procedure TGQueue.unlink(link_: PGList); cdecl;
begin
  GLib2.g_queue_unlink(@self, link_);
end;

function TGQueue.new: PGQueue; cdecl;
begin
  Result := GLib2.g_queue_new();
end;

function TGRand.copy: PGRand; cdecl;
begin
  Result := GLib2.g_rand_copy(@self);
end;

function TGRand.double: gdouble; cdecl;
begin
  Result := GLib2.g_rand_double(@self);
end;

function TGRand.double_range(begin_: gdouble; end_: gdouble): gdouble; cdecl;
begin
  Result := GLib2.g_rand_double_range(@self, begin_, end_);
end;

procedure TGRand.free; cdecl;
begin
  GLib2.g_rand_free(@self);
end;

function TGRand.int: guint32; cdecl;
begin
  Result := GLib2.g_rand_int(@self);
end;

function TGRand.int_range(begin_: gint32; end_: gint32): gint32; cdecl;
begin
  Result := GLib2.g_rand_int_range(@self, begin_, end_);
end;

procedure TGRand.set_seed(seed: guint32); cdecl;
begin
  GLib2.g_rand_set_seed(@self, seed);
end;

procedure TGRand.set_seed_array(seed: Pguint32; seed_length: guint); cdecl;
begin
  GLib2.g_rand_set_seed_array(@self, seed, seed_length);
end;

function TGRand.new: PGRand; cdecl;
begin
  Result := GLib2.g_rand_new();
end;

function TGRand.new_with_seed(seed: guint32): PGRand; cdecl;
begin
  Result := GLib2.g_rand_new_with_seed(seed);
end;

function TGRand.new_with_seed_array(seed: Pguint32; seed_length: guint): PGRand; cdecl;
begin
  Result := GLib2.g_rand_new_with_seed_array(seed, seed_length);
end;

function TGScanner.cur_line: guint; cdecl;
begin
  Result := GLib2.g_scanner_cur_line(@self);
end;

function TGScanner.cur_position: guint; cdecl;
begin
  Result := GLib2.g_scanner_cur_position(@self);
end;

function TGScanner.cur_token: TGTokenType; cdecl;
begin
  Result := GLib2.g_scanner_cur_token(@self);
end;

function TGScanner.cur_value: TGTokenValue; cdecl;
begin
  Result := GLib2.g_scanner_cur_value(@self);
end;

procedure TGScanner.destroy_; cdecl;
begin
  GLib2.g_scanner_destroy(@self);
end;

function TGScanner.eof: gboolean; cdecl;
begin
  Result := GLib2.g_scanner_eof(@self);
end;

function TGScanner.get_next_token: TGTokenType; cdecl;
begin
  Result := GLib2.g_scanner_get_next_token(@self);
end;

procedure TGScanner.input_file(input_fd: gint); cdecl;
begin
  GLib2.g_scanner_input_file(@self, input_fd);
end;

procedure TGScanner.input_text(text: Pgchar; text_len: guint); cdecl;
begin
  GLib2.g_scanner_input_text(@self, text, text_len);
end;

function TGScanner.lookup_symbol(symbol: Pgchar): gpointer; cdecl;
begin
  Result := GLib2.g_scanner_lookup_symbol(@self, symbol);
end;

function TGScanner.peek_next_token: TGTokenType; cdecl;
begin
  Result := GLib2.g_scanner_peek_next_token(@self);
end;

procedure TGScanner.scope_add_symbol(scope_id: guint; symbol: Pgchar; value: gpointer); cdecl;
begin
  GLib2.g_scanner_scope_add_symbol(@self, scope_id, symbol, value);
end;

procedure TGScanner.scope_foreach_symbol(scope_id: guint; func: TGHFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_scanner_scope_foreach_symbol(@self, scope_id, func, user_data);
end;

function TGScanner.scope_lookup_symbol(scope_id: guint; symbol: Pgchar): gpointer; cdecl;
begin
  Result := GLib2.g_scanner_scope_lookup_symbol(@self, scope_id, symbol);
end;

procedure TGScanner.scope_remove_symbol(scope_id: guint; symbol: Pgchar); cdecl;
begin
  GLib2.g_scanner_scope_remove_symbol(@self, scope_id, symbol);
end;

function TGScanner.set_scope(scope_id: guint): guint; cdecl;
begin
  Result := GLib2.g_scanner_set_scope(@self, scope_id);
end;

procedure TGScanner.sync_file_offset; cdecl;
begin
  GLib2.g_scanner_sync_file_offset(@self);
end;

procedure TGScanner.unexp_token(expected_token: TGTokenType; identifier_spec: Pgchar; symbol_spec: Pgchar; symbol_name: Pgchar; message: Pgchar; is_error: gint); cdecl;
begin
  GLib2.g_scanner_unexp_token(@self, expected_token, identifier_spec, symbol_spec, symbol_name, message, is_error);
end;

function TGScanner.new(config_templ: PGScannerConfig): PGScanner; cdecl;
begin
  Result := GLib2.g_scanner_new(config_templ);
end;

function TGSequenceIter.compare(b: PGSequenceIter): gint; cdecl;
begin
  Result := GLib2.g_sequence_iter_compare(@self, b);
end;

function TGSequenceIter.get_position: gint; cdecl;
begin
  Result := GLib2.g_sequence_iter_get_position(@self);
end;

function TGSequenceIter.get_sequence: PGSequence; cdecl;
begin
  Result := GLib2.g_sequence_iter_get_sequence(@self);
end;

function TGSequenceIter.is_begin: gboolean; cdecl;
begin
  Result := GLib2.g_sequence_iter_is_begin(@self);
end;

function TGSequenceIter.is_end: gboolean; cdecl;
begin
  Result := GLib2.g_sequence_iter_is_end(@self);
end;

function TGSequenceIter.move(delta: gint): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_iter_move(@self, delta);
end;

function TGSequenceIter.next: PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_iter_next(@self);
end;

function TGSequenceIter.prev: PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_iter_prev(@self);
end;

function TGSequence.append(data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_append(@self, data);
end;

procedure TGSequence.foreach(func: TGFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_sequence_foreach(@self, func, user_data);
end;

procedure TGSequence.free; cdecl;
begin
  GLib2.g_sequence_free(@self);
end;

function TGSequence.get_begin_iter: PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_get_begin_iter(@self);
end;

function TGSequence.get_end_iter: PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_get_end_iter(@self);
end;

function TGSequence.get_iter_at_pos(pos: gint): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_get_iter_at_pos(@self, pos);
end;

function TGSequence.get_length: gint; cdecl;
begin
  Result := GLib2.g_sequence_get_length(@self);
end;

function TGSequence.insert_sorted(data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_insert_sorted(@self, data, cmp_func, cmp_data);
end;

function TGSequence.insert_sorted_iter(data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_insert_sorted_iter(@self, data, iter_cmp, cmp_data);
end;

function TGSequence.lookup(data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_lookup(@self, data, cmp_func, cmp_data);
end;

function TGSequence.lookup_iter(data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_lookup_iter(@self, data, iter_cmp, cmp_data);
end;

function TGSequence.prepend(data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_prepend(@self, data);
end;

function TGSequence.search(data: gpointer; cmp_func: TGCompareDataFunc; cmp_data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_search(@self, data, cmp_func, cmp_data);
end;

function TGSequence.search_iter(data: gpointer; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_search_iter(@self, data, iter_cmp, cmp_data);
end;

procedure TGSequence.sort(cmp_func: TGCompareDataFunc; cmp_data: gpointer); cdecl;
begin
  GLib2.g_sequence_sort(@self, cmp_func, cmp_data);
end;

procedure TGSequence.sort_iter(cmp_func: TGSequenceIterCompareFunc; cmp_data: gpointer); cdecl;
begin
  GLib2.g_sequence_sort_iter(@self, cmp_func, cmp_data);
end;

procedure TGSequence.foreach_range(begin_: PGSequenceIter; end_: PGSequenceIter; func: TGFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_sequence_foreach_range(begin_, end_, func, user_data);
end;

function TGSequence.get(iter: PGSequenceIter): gpointer; cdecl;
begin
  Result := GLib2.g_sequence_get(iter);
end;

function TGSequence.insert_before(iter: PGSequenceIter; data: gpointer): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_insert_before(iter, data);
end;

procedure TGSequence.move(src: PGSequenceIter; dest: PGSequenceIter); cdecl;
begin
  GLib2.g_sequence_move(src, dest);
end;

procedure TGSequence.move_range(dest: PGSequenceIter; begin_: PGSequenceIter; end_: PGSequenceIter); cdecl;
begin
  GLib2.g_sequence_move_range(dest, begin_, end_);
end;

function TGSequence.new(data_destroy: TGDestroyNotify): PGSequence; cdecl;
begin
  Result := GLib2.g_sequence_new(data_destroy);
end;

function TGSequence.range_get_midpoint(begin_: PGSequenceIter; end_: PGSequenceIter): PGSequenceIter; cdecl;
begin
  Result := GLib2.g_sequence_range_get_midpoint(begin_, end_);
end;

procedure TGSequence.remove(iter: PGSequenceIter); cdecl;
begin
  GLib2.g_sequence_remove(iter);
end;

procedure TGSequence.remove_range(begin_: PGSequenceIter; end_: PGSequenceIter); cdecl;
begin
  GLib2.g_sequence_remove_range(begin_, end_);
end;

procedure TGSequence.set_(iter: PGSequenceIter; data: gpointer); cdecl;
begin
  GLib2.g_sequence_set(iter, data);
end;

procedure TGSequence.sort_changed(iter: PGSequenceIter; cmp_func: TGCompareDataFunc; cmp_data: gpointer); cdecl;
begin
  GLib2.g_sequence_sort_changed(iter, cmp_func, cmp_data);
end;

procedure TGSequence.sort_changed_iter(iter: PGSequenceIter; iter_cmp: TGSequenceIterCompareFunc; cmp_data: gpointer); cdecl;
begin
  GLib2.g_sequence_sort_changed_iter(iter, iter_cmp, cmp_data);
end;

procedure TGSequence.swap(a: PGSequenceIter; b: PGSequenceIter); cdecl;
begin
  GLib2.g_sequence_swap(a, b);
end;


procedure TGStaticMutex.free; cdecl;
begin
  GLib2.g_static_mutex_free(@self);
end;

procedure TGStaticMutex.init; cdecl;
begin
  GLib2.g_static_mutex_init(@self);
end;

function TGStaticMutex.get_mutex_impl(mutex: PPGMutex): PGMutex; cdecl;
begin
  Result := GLib2.g_static_mutex_get_mutex_impl(mutex);
end;

procedure TGStaticPrivate.free; cdecl;
begin
  GLib2.g_static_private_free(@self);
end;

function TGStaticPrivate.get: gpointer; cdecl;
begin
  Result := GLib2.g_static_private_get(@self);
end;

procedure TGStaticPrivate.init; cdecl;
begin
  GLib2.g_static_private_init(@self);
end;

procedure TGStaticPrivate.set_(data: gpointer; notify: TGDestroyNotify); cdecl;
begin
  GLib2.g_static_private_set(@self, data, notify);
end;

procedure TGStaticRWLock.free; cdecl;
begin
  GLib2.g_static_rw_lock_free(@self);
end;

procedure TGStaticRWLock.init; cdecl;
begin
  GLib2.g_static_rw_lock_init(@self);
end;

procedure TGStaticRWLock.reader_lock; cdecl;
begin
  GLib2.g_static_rw_lock_reader_lock(@self);
end;

function TGStaticRWLock.reader_trylock: gboolean; cdecl;
begin
  Result := GLib2.g_static_rw_lock_reader_trylock(@self);
end;

procedure TGStaticRWLock.reader_unlock; cdecl;
begin
  GLib2.g_static_rw_lock_reader_unlock(@self);
end;

procedure TGStaticRWLock.writer_lock; cdecl;
begin
  GLib2.g_static_rw_lock_writer_lock(@self);
end;

function TGStaticRWLock.writer_trylock: gboolean; cdecl;
begin
  Result := GLib2.g_static_rw_lock_writer_trylock(@self);
end;

procedure TGStaticRWLock.writer_unlock; cdecl;
begin
  GLib2.g_static_rw_lock_writer_unlock(@self);
end;

procedure TGStaticRecMutex.free; cdecl;
begin
  GLib2.g_static_rec_mutex_free(@self);
end;

procedure TGStaticRecMutex.init; cdecl;
begin
  GLib2.g_static_rec_mutex_init(@self);
end;

procedure TGStaticRecMutex.lock; cdecl;
begin
  GLib2.g_static_rec_mutex_lock(@self);
end;

procedure TGStaticRecMutex.lock_full(depth: guint); cdecl;
begin
  GLib2.g_static_rec_mutex_lock_full(@self, depth);
end;

function TGStaticRecMutex.trylock: gboolean; cdecl;
begin
  Result := GLib2.g_static_rec_mutex_trylock(@self);
end;

procedure TGStaticRecMutex.unlock; cdecl;
begin
  GLib2.g_static_rec_mutex_unlock(@self);
end;

function TGStaticRecMutex.unlock_full: guint; cdecl;
begin
  Result := GLib2.g_static_rec_mutex_unlock_full(@self);
end;

procedure TGStringChunk.clear; cdecl;
begin
  GLib2.g_string_chunk_clear(@self);
end;

procedure TGStringChunk.free; cdecl;
begin
  GLib2.g_string_chunk_free(@self);
end;

function TGStringChunk.insert(string_: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_string_chunk_insert(@self, string_);
end;

function TGStringChunk.insert_const(string_: Pgchar): Pgchar; cdecl;
begin
  Result := GLib2.g_string_chunk_insert_const(@self, string_);
end;

function TGStringChunk.insert_len(string_: Pgchar; len: gssize): Pgchar; cdecl;
begin
  Result := GLib2.g_string_chunk_insert_len(@self, string_, len);
end;

function TGStringChunk.new(size: gsize): PGStringChunk; cdecl;
begin
  Result := GLib2.g_string_chunk_new(size);
end;

procedure TGTestLogMsg.free; cdecl;
begin
  GLib2.g_test_log_msg_free(@self);
end;

procedure TGTestLogBuffer.free; cdecl;
begin
  GLib2.g_test_log_buffer_free(@self);
end;

function TGTestLogBuffer.pop: PGTestLogMsg; cdecl;
begin
  Result := GLib2.g_test_log_buffer_pop(@self);
end;

procedure TGTestLogBuffer.push(n_bytes: guint; bytes: Pguint8); cdecl;
begin
  GLib2.g_test_log_buffer_push(@self, n_bytes, bytes);
end;

function TGTestLogBuffer.new: PGTestLogBuffer; cdecl;
begin
  Result := GLib2.g_test_log_buffer_new();
end;

procedure TGTestSuite.add(test_case: PGTestCase); cdecl;
begin
  GLib2.g_test_suite_add(@self, test_case);
end;

procedure TGTestSuite.add_suite(nestedsuite: PGTestSuite); cdecl;
begin
  GLib2.g_test_suite_add_suite(@self, nestedsuite);
end;


function TGThread.join: gpointer; cdecl;
begin
  Result := GLib2.g_thread_join(@self);
end;

procedure TGThread.set_priority(priority: TGThreadPriority); cdecl;
begin
  GLib2.g_thread_set_priority(@self, priority);
end;

function TGThread.create_full(func: TGThreadFunc; data: gpointer; stack_size: gulong; joinable: gboolean; bound: gboolean; priority: TGThreadPriority): PGThread; cdecl;
begin
  Result := GLib2.g_thread_create_full(func, data, stack_size, joinable, bound, priority);
end;

function TGThread.error_quark: TGQuark; cdecl;
begin
  Result := GLib2.g_thread_error_quark();
end;

procedure TGThread.exit(retval: gpointer); cdecl;
begin
  GLib2.g_thread_exit(retval);
end;

procedure TGThread.foreach(thread_func: TGFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_thread_foreach(thread_func, user_data);
end;

function TGThread.get_initialized: gboolean; cdecl;
begin
  Result := GLib2.g_thread_get_initialized();
end;

procedure TGThread.init(vtable: PGThreadFunctions); cdecl;
begin
  GLib2.g_thread_init(vtable);
end;

procedure TGThread.init_with_errorcheck_mutexes(vtable: PGThreadFunctions); cdecl;
begin
  GLib2.g_thread_init_with_errorcheck_mutexes(vtable);
end;

function TGThread.self: PGThread; cdecl;
begin
  Result := GLib2.g_thread_self();
end;

procedure TGThreadPool.free(immediate: gboolean; wait_: gboolean); cdecl;
begin
  GLib2.g_thread_pool_free(@self, immediate, wait_);
end;

function TGThreadPool.get_max_threads: gint; cdecl;
begin
  Result := GLib2.g_thread_pool_get_max_threads(@self);
end;

function TGThreadPool.get_num_threads: guint; cdecl;
begin
  Result := GLib2.g_thread_pool_get_num_threads(@self);
end;

procedure TGThreadPool.push(data: gpointer); cdecl;
begin
  GLib2.g_thread_pool_push(@self, data);
end;

procedure TGThreadPool.set_max_threads(max_threads: gint); cdecl;
begin
  GLib2.g_thread_pool_set_max_threads(@self, max_threads);
end;

procedure TGThreadPool.set_sort_function(func: TGCompareDataFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_thread_pool_set_sort_function(@self, func, user_data);
end;

function TGThreadPool.unprocessed: guint; cdecl;
begin
  Result := GLib2.g_thread_pool_unprocessed(@self);
end;

function TGThreadPool.get_max_idle_time: guint; cdecl;
begin
  Result := GLib2.g_thread_pool_get_max_idle_time();
end;

function TGThreadPool.get_max_unused_threads: gint; cdecl;
begin
  Result := GLib2.g_thread_pool_get_max_unused_threads();
end;

function TGThreadPool.get_num_unused_threads: guint; cdecl;
begin
  Result := GLib2.g_thread_pool_get_num_unused_threads();
end;

function TGThreadPool.new(func: TGFunc; user_data: gpointer; max_threads: gint; exclusive: gboolean): PGThreadPool; cdecl;
begin
  Result := GLib2.g_thread_pool_new(func, user_data, max_threads, exclusive);
end;

procedure TGThreadPool.set_max_idle_time(interval: guint); cdecl;
begin
  GLib2.g_thread_pool_set_max_idle_time(interval);
end;

procedure TGThreadPool.set_max_unused_threads(max_threads: gint); cdecl;
begin
  GLib2.g_thread_pool_set_max_unused_threads(max_threads);
end;

procedure TGThreadPool.stop_unused_threads; cdecl;
begin
  GLib2.g_thread_pool_stop_unused_threads();
end;

procedure TGTimer.continue; cdecl;
begin
  GLib2.g_timer_continue(@self);
end;

procedure TGTimer.destroy_; cdecl;
begin
  GLib2.g_timer_destroy(@self);
end;

function TGTimer.elapsed(microseconds: Pgulong): gdouble; cdecl;
begin
  Result := GLib2.g_timer_elapsed(@self, microseconds);
end;

procedure TGTimer.reset; cdecl;
begin
  GLib2.g_timer_reset(@self);
end;

procedure TGTimer.start; cdecl;
begin
  GLib2.g_timer_start(@self);
end;

procedure TGTimer.stop; cdecl;
begin
  GLib2.g_timer_stop(@self);
end;

function TGTimer.new: PGTimer; cdecl;
begin
  Result := GLib2.g_timer_new();
end;

function TGTrashStack.height(stack_p: PPGTrashStack): guint; cdecl;
begin
  Result := GLib2.g_trash_stack_height(stack_p);
end;

function TGTrashStack.peek(stack_p: PPGTrashStack): gpointer; cdecl;
begin
  Result := GLib2.g_trash_stack_peek(stack_p);
end;

function TGTrashStack.pop(stack_p: PPGTrashStack): gpointer; cdecl;
begin
  Result := GLib2.g_trash_stack_pop(stack_p);
end;

procedure TGTrashStack.push(stack_p: PPGTrashStack; data_p: gpointer); cdecl;
begin
  GLib2.g_trash_stack_push(stack_p, data_p);
end;

procedure TGTree.destroy_; cdecl;
begin
  GLib2.g_tree_destroy(@self);
end;

procedure TGTree.foreach(func: TGTraverseFunc; user_data: gpointer); cdecl;
begin
  GLib2.g_tree_foreach(@self, func, user_data);
end;

function TGTree.height: gint; cdecl;
begin
  Result := GLib2.g_tree_height(@self);
end;

procedure TGTree.insert(key: gpointer; value: gpointer); cdecl;
begin
  GLib2.g_tree_insert(@self, key, value);
end;

function TGTree.lookup(key: gpointer): gpointer; cdecl;
begin
  Result := GLib2.g_tree_lookup(@self, key);
end;

function TGTree.lookup_extended(lookup_key: gpointer; orig_key: Pgpointer; value: Pgpointer): gboolean; cdecl;
begin
  Result := GLib2.g_tree_lookup_extended(@self, lookup_key, orig_key, value);
end;

function TGTree.nnodes: gint; cdecl;
begin
  Result := GLib2.g_tree_nnodes(@self);
end;

function TGTree.ref: PGTree; cdecl;
begin
  Result := GLib2.g_tree_ref(@self);
end;

function TGTree.remove(key: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_tree_remove(@self, key);
end;

procedure TGTree.replace(key: gpointer; value: gpointer); cdecl;
begin
  GLib2.g_tree_replace(@self, key, value);
end;

function TGTree.search(search_func: TGCompareFunc; user_data: gpointer): gpointer; cdecl;
begin
  Result := GLib2.g_tree_search(@self, search_func, user_data);
end;

function TGTree.steal(key: gpointer): gboolean; cdecl;
begin
  Result := GLib2.g_tree_steal(@self, key);
end;

procedure TGTree.traverse(traverse_func: TGTraverseFunc; traverse_type: TGTraverseType; user_data: gpointer); cdecl;
begin
  GLib2.g_tree_traverse(@self, traverse_func, traverse_type, user_data);
end;

procedure TGTree.unref; cdecl;
begin
  GLib2.g_tree_unref(@self);
end;

function TGTree.new(key_compare_func: TGCompareFunc): PGTree; cdecl;
begin
  Result := GLib2.g_tree_new(key_compare_func);
end;

function TGTree.new_full(key_compare_func: TGCompareDataFunc; key_compare_data: gpointer; key_destroy_func: TGDestroyNotify; value_destroy_func: TGDestroyNotify): PGTree; cdecl;
begin
  Result := GLib2.g_tree_new_full(key_compare_func, key_compare_data, key_destroy_func, value_destroy_func);
end;

function TGTree.new_with_data(key_compare_func: TGCompareDataFunc; key_compare_data: gpointer): PGTree; cdecl;
begin
  Result := GLib2.g_tree_new_with_data(key_compare_func, key_compare_data);
end;

function TGVariant.new_array(child_type: PGVariantType; children: PPGVariant; n_children: gsize): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_array(child_type, children, n_children);
end;

function TGVariant.new_boolean(value: gboolean): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_boolean(value);
end;

function TGVariant.new_byte(value: guint8): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_byte(value);
end;

function TGVariant.new_bytestring(string_: Pgchar): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_bytestring(string_);
end;

function TGVariant.new_bytestring_array(strv: PPgchar; length: gssize): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_bytestring_array(strv, length);
end;

function TGVariant.new_dict_entry(key: PGVariant; value: PGVariant): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_dict_entry(key, value);
end;

function TGVariant.new_double(value: gdouble): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_double(value);
end;

function TGVariant.new_from_data(type_: PGVariantType; data: guint8; size: gsize; trusted: gboolean; notify: TGDestroyNotify; user_data: gpointer): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_from_data(type_, data, size, trusted, notify, user_data);
end;

function TGVariant.new_handle(value: gint32): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_handle(value);
end;

function TGVariant.new_int16(value: gint16): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_int16(value);
end;

function TGVariant.new_int32(value: gint32): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_int32(value);
end;

function TGVariant.new_int64(value: gint64): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_int64(value);
end;

function TGVariant.new_maybe(child_type: PGVariantType; child: PGVariant): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_maybe(child_type, child);
end;

function TGVariant.new_object_path(object_path: Pgchar): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_object_path(object_path);
end;

function TGVariant.new_objv(strv: PPgchar; length: gssize): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_objv(strv, length);
end;

function TGVariant.new_signature(signature: Pgchar): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_signature(signature);
end;

function TGVariant.new_string(string_: Pgchar): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_string(string_);
end;

function TGVariant.new_strv(strv: PPgchar; length: gssize): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_strv(strv, length);
end;

function TGVariant.new_tuple(children: PPGVariant; n_children: gsize): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_tuple(children, n_children);
end;

function TGVariant.new_uint16(value: guint16): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_uint16(value);
end;

function TGVariant.new_uint32(value: guint32): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_uint32(value);
end;

function TGVariant.new_uint64(value: guint64): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_uint64(value);
end;

function TGVariant.new_variant(value: PGVariant): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_new_variant(value);
end;

function TGVariant.byteswap: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_byteswap(@self);
end;

function TGVariant.classify: TGVariantClass; cdecl;
begin
  Result := GLib2.g_variant_classify(@self);
end;

function TGVariant.compare(two: TGVariant): gint; cdecl;
begin
  Result := GLib2.g_variant_compare(@self, two);
end;

function TGVariant.dup_bytestring(length: Pgsize): Pgchar; cdecl;
begin
  Result := GLib2.g_variant_dup_bytestring(@self, length);
end;

function TGVariant.dup_bytestring_array(length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_variant_dup_bytestring_array(@self, length);
end;

function TGVariant.dup_objv(length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_variant_dup_objv(@self, length);
end;

function TGVariant.dup_string(length: Pgsize): Pgchar; cdecl;
begin
  Result := GLib2.g_variant_dup_string(@self, length);
end;

function TGVariant.dup_strv(length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_variant_dup_strv(@self, length);
end;

function TGVariant.equal(two: TGVariant): gboolean; cdecl;
begin
  Result := GLib2.g_variant_equal(@self, two);
end;

function TGVariant.get_boolean: gboolean; cdecl;
begin
  Result := GLib2.g_variant_get_boolean(@self);
end;

function TGVariant.get_byte: guint8; cdecl;
begin
  Result := GLib2.g_variant_get_byte(@self);
end;

function TGVariant.get_bytestring: Pgchar; cdecl;
begin
  Result := GLib2.g_variant_get_bytestring(@self);
end;

function TGVariant.get_bytestring_array(length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_variant_get_bytestring_array(@self, length);
end;

function TGVariant.get_child_value(index_: gsize): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_get_child_value(@self, index_);
end;

function TGVariant.get_data: gpointer; cdecl;
begin
  Result := GLib2.g_variant_get_data(@self);
end;

function TGVariant.get_double: gdouble; cdecl;
begin
  Result := GLib2.g_variant_get_double(@self);
end;

function TGVariant.get_fixed_array(n_elements: Pgsize; element_size: gsize): gpointer; cdecl;
begin
  Result := GLib2.g_variant_get_fixed_array(@self, n_elements, element_size);
end;

function TGVariant.get_handle: gint32; cdecl;
begin
  Result := GLib2.g_variant_get_handle(@self);
end;

function TGVariant.get_int16: gint16; cdecl;
begin
  Result := GLib2.g_variant_get_int16(@self);
end;

function TGVariant.get_int32: gint32; cdecl;
begin
  Result := GLib2.g_variant_get_int32(@self);
end;

function TGVariant.get_int64: gint64; cdecl;
begin
  Result := GLib2.g_variant_get_int64(@self);
end;

function TGVariant.get_maybe: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_get_maybe(@self);
end;

function TGVariant.get_normal_form: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_get_normal_form(@self);
end;

function TGVariant.get_objv(length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_variant_get_objv(@self, length);
end;

function TGVariant.get_size: gsize; cdecl;
begin
  Result := GLib2.g_variant_get_size(@self);
end;

function TGVariant.get_string(length: Pgsize): Pgchar; cdecl;
begin
  Result := GLib2.g_variant_get_string(@self, length);
end;

function TGVariant.get_strv(length: Pgsize): PPgchar; cdecl;
begin
  Result := GLib2.g_variant_get_strv(@self, length);
end;

function TGVariant.get_type_string: Pgchar; cdecl;
begin
  Result := GLib2.g_variant_get_type_string(@self);
end;

function TGVariant.get_uint16: guint16; cdecl;
begin
  Result := GLib2.g_variant_get_uint16(@self);
end;

function TGVariant.get_uint32: guint32; cdecl;
begin
  Result := GLib2.g_variant_get_uint32(@self);
end;

function TGVariant.get_uint64: guint64; cdecl;
begin
  Result := GLib2.g_variant_get_uint64(@self);
end;

function TGVariant.get_variant: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_get_variant(@self);
end;

function TGVariant.hash: guint; cdecl;
begin
  Result := GLib2.g_variant_hash(@self);
end;

function TGVariant.is_container: gboolean; cdecl;
begin
  Result := GLib2.g_variant_is_container(@self);
end;

function TGVariant.is_floating: gboolean; cdecl;
begin
  Result := GLib2.g_variant_is_floating(@self);
end;

function TGVariant.is_normal_form: gboolean; cdecl;
begin
  Result := GLib2.g_variant_is_normal_form(@self);
end;

function TGVariant.is_of_type(type_: PGVariantType): gboolean; cdecl;
begin
  Result := GLib2.g_variant_is_of_type(@self, type_);
end;

function TGVariant.iter_new: PGVariantIter; cdecl;
begin
  Result := GLib2.g_variant_iter_new(@self);
end;

function TGVariant.lookup_value(key: Pgchar; expected_type: PGVariantType): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_lookup_value(@self, key, expected_type);
end;

function TGVariant.n_children: gsize; cdecl;
begin
  Result := GLib2.g_variant_n_children(@self);
end;

function TGVariant.print(type_annotate: gboolean): Pgchar; cdecl;
begin
  Result := GLib2.g_variant_print(@self, type_annotate);
end;

function TGVariant.print_string(string_: PGString; type_annotate: gboolean): PGString; cdecl;
begin
  Result := GLib2.g_variant_print_string(@self, string_, type_annotate);
end;

function TGVariant.ref: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_ref(@self);
end;

function TGVariant.ref_sink: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_ref_sink(@self);
end;

procedure TGVariant.store(data: gpointer); cdecl;
begin
  GLib2.g_variant_store(@self, data);
end;

function TGVariant.take_ref: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_take_ref(@self);
end;

procedure TGVariant.unref; cdecl;
begin
  GLib2.g_variant_unref(@self);
end;

function TGVariant.is_object_path(string_: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_variant_is_object_path(string_);
end;

function TGVariant.is_signature(string_: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_variant_is_signature(string_);
end;

function TGVariant.parse(type_: PGVariantType; text: Pgchar; limit: Pgchar; endptr: PPgchar): PGVariant; cdecl;
begin
  Result := GLib2.g_variant_parse(type_, text, limit, endptr);
end;

function TGVariant.parser_get_error_quark: TGQuark; cdecl;
begin
  Result := GLib2.g_variant_parser_get_error_quark();
end;

function TGVariantType.new(type_string: Pgchar): PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_new(type_string);
end;

function TGVariantType.new_tuple(items: PPGVariantType; length: gint): PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_new_tuple(items, length);
end;

function TGVariantType.copy: PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_copy(@self);
end;

function TGVariantType.dup_string: Pgchar; cdecl;
begin
  Result := GLib2.g_variant_type_dup_string(@self);
end;

function TGVariantType.element: PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_element(@self);
end;

function TGVariantType.equal(type2: TGVariantType): gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_equal(@self, type2);
end;

function TGVariantType.first: PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_first(@self);
end;

procedure TGVariantType.free; cdecl;
begin
  GLib2.g_variant_type_free(@self);
end;

function TGVariantType.get_string_length: gsize; cdecl;
begin
  Result := GLib2.g_variant_type_get_string_length(@self);
end;

function TGVariantType.hash: guint; cdecl;
begin
  Result := GLib2.g_variant_type_hash(@self);
end;

function TGVariantType.is_array: gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_array(@self);
end;

function TGVariantType.is_basic: gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_basic(@self);
end;

function TGVariantType.is_container: gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_container(@self);
end;

function TGVariantType.is_definite: gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_definite(@self);
end;

function TGVariantType.is_dict_entry: gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_dict_entry(@self);
end;

function TGVariantType.is_maybe: gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_maybe(@self);
end;

function TGVariantType.is_subtype_of(supertype: PGVariantType): gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_subtype_of(@self, supertype);
end;

function TGVariantType.is_tuple: gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_tuple(@self);
end;

function TGVariantType.is_variant: gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_is_variant(@self);
end;

function TGVariantType.key: PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_key(@self);
end;

function TGVariantType.n_items: gsize; cdecl;
begin
  Result := GLib2.g_variant_type_n_items(@self);
end;

function TGVariantType.new_array: PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_new_array(@self);
end;

function TGVariantType.new_dict_entry(value: PGVariantType): PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_new_dict_entry(@self, value);
end;

function TGVariantType.new_maybe: PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_new_maybe(@self);
end;

function TGVariantType.next: PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_next(@self);
end;

function TGVariantType.peek_string: Pgchar; cdecl;
begin
  Result := GLib2.g_variant_type_peek_string(@self);
end;

function TGVariantType.value: PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_value(@self);
end;

function TGVariantType.checked_(param0: Pgchar): PGVariantType; cdecl;
begin
  Result := GLib2.g_variant_type_checked_(param0);
end;

function TGVariantType.string_is_valid(type_string: Pgchar): gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_string_is_valid(type_string);
end;

function TGVariantType.string_scan(string_: Pgchar; limit: Pgchar; endptr: PPgchar): gboolean; cdecl;
begin
  Result := GLib2.g_variant_type_string_scan(string_, limit, endptr);
end;

function TGVariantIter.copy: PGVariantIter; cdecl;
begin
  Result := GLib2.g_variant_iter_copy(@self);
end;

procedure TGVariantIter.free; cdecl;
begin
  GLib2.g_variant_iter_free(@self);
end;

function TGVariantIter.init(value: PGVariant): gsize; cdecl;
begin
  Result := GLib2.g_variant_iter_init(@self, value);
end;

function TGVariantIter.n_children: gsize; cdecl;
begin
  Result := GLib2.g_variant_iter_n_children(@self);
end;

function TGVariantIter.next_value: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_iter_next_value(@self);
end;

function TGVariantBuilder.new(type_: PGVariantType): PGVariantBuilder; cdecl;
begin
  Result := GLib2.g_variant_builder_new(type_);
end;

procedure TGVariantBuilder.add_value(value: PGVariant); cdecl;
begin
  GLib2.g_variant_builder_add_value(@self, value);
end;

procedure TGVariantBuilder.clear; cdecl;
begin
  GLib2.g_variant_builder_clear(@self);
end;

procedure TGVariantBuilder.close; cdecl;
begin
  GLib2.g_variant_builder_close(@self);
end;

function TGVariantBuilder.end_: PGVariant; cdecl;
begin
  Result := GLib2.g_variant_builder_end(@self);
end;

procedure TGVariantBuilder.init(type_: PGVariantType); cdecl;
begin
  GLib2.g_variant_builder_init(@self, type_);
end;

procedure TGVariantBuilder.open(type_: PGVariantType); cdecl;
begin
  GLib2.g_variant_builder_open(@self, type_);
end;

function TGVariantBuilder.ref: PGVariantBuilder; cdecl;
begin
  Result := GLib2.g_variant_builder_ref(@self);
end;

procedure TGVariantBuilder.unref; cdecl;
begin
  GLib2.g_variant_builder_unref(@self);
end;

end.