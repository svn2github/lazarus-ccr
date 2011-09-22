unit WebKit3;

{$MODE OBJFPC}{$H+}

{$PACKRECORDS C}
{$BITPACKING ON}
{$MODESWITCH DUPLICATELOCALS+}

{$LINKLIB libwebkitgtk-3.0.so.0}
interface
uses
  CTypes, Atk1, GLib2, GModule2, GObject2, Gdk3, GdkPixbuf2, Gio2, Gtk3, JSCore3, Pango1, Soup2_4, cairo1;

const
  WebKit3_library = 'libwebkitgtk-3.0.so.0';

  MAJOR_VERSION = 1;
  MICRO_VERSION = 2;
  MINOR_VERSION = 4;
  USER_AGENT_MAJOR_VERSION = 534;
  USER_AGENT_MINOR_VERSION = 26;

type
  TWebKitCacheModel = Integer;
const
  { WebKitCacheModel }
  WEBKIT_CACHE_MODEL_DEFAULT: TWebKitCacheModel = 0;
  WEBKIT_CACHE_MODEL_DOCUMENT_VIEWER: TWebKitCacheModel = 1;
  WEBKIT_CACHE_MODEL_WEB_BROWSER: TWebKitCacheModel = 2;
  WEBKIT_CACHE_MODEL_DOCUMENT_BROWSER: TWebKitCacheModel = 3;

type
  TWebKitDownloadStatus = Integer;
const
  { WebKitDownloadStatus }
  WEBKIT_DOWNLOAD_STATUS_ERROR: TWebKitDownloadStatus = -1;
  WEBKIT_DOWNLOAD_STATUS_CREATED: TWebKitDownloadStatus = 0;
  WEBKIT_DOWNLOAD_STATUS_STARTED: TWebKitDownloadStatus = 1;
  WEBKIT_DOWNLOAD_STATUS_CANCELLED: TWebKitDownloadStatus = 2;
  WEBKIT_DOWNLOAD_STATUS_FINISHED: TWebKitDownloadStatus = 3;

type
  TWebKitDownloadError = Integer;
const
  { WebKitDownloadError }
  WEBKIT_DOWNLOAD_ERROR_CANCELLED_BY_USER: TWebKitDownloadError = 0;
  WEBKIT_DOWNLOAD_ERROR_DESTINATION: TWebKitDownloadError = 1;
  WEBKIT_DOWNLOAD_ERROR_NETWORK: TWebKitDownloadError = 2;

type
  TWebKitEditingBehavior = Integer;
const
  { WebKitEditingBehavior }
  WEBKIT_EDITING_BEHAVIOR_MAC: TWebKitEditingBehavior = 0;
  WEBKIT_EDITING_BEHAVIOR_WINDOWS: TWebKitEditingBehavior = 1;
  WEBKIT_EDITING_BEHAVIOR_UNIX: TWebKitEditingBehavior = 2;

  { WebKitHitTestResultContext }
  WEBKIT_HIT_TEST_RESULT_CONTEXT_DOCUMENT = 2;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_LINK = 4;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_IMAGE = 8;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_MEDIA = 16;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_SELECTION = 32;
  WEBKIT_HIT_TEST_RESULT_CONTEXT_EDITABLE = 64;

type
  TWebKitInsertAction = Integer;
const
  { WebKitInsertAction }
  WEBKIT_INSERT_ACTION_TYPED: TWebKitInsertAction = 0;
  WEBKIT_INSERT_ACTION_PASTED: TWebKitInsertAction = 1;
  WEBKIT_INSERT_ACTION_DROPPED: TWebKitInsertAction = 2;

type
  TWebKitLoadStatus = Integer;
const
  { WebKitLoadStatus }
  WEBKIT_LOAD_PROVISIONAL: TWebKitLoadStatus = 0;
  WEBKIT_LOAD_COMMITTED: TWebKitLoadStatus = 1;
  WEBKIT_LOAD_FINISHED: TWebKitLoadStatus = 2;
  WEBKIT_LOAD_FIRST_VISUALLY_NON_EMPTY_LAYOUT: TWebKitLoadStatus = 3;
  WEBKIT_LOAD_FAILED: TWebKitLoadStatus = 4;

type
  TWebKitNavigationResponse = Integer;
const
  { WebKitNavigationResponse }
  WEBKIT_NAVIGATION_RESPONSE_ACCEPT: TWebKitNavigationResponse = 0;
  WEBKIT_NAVIGATION_RESPONSE_IGNORE: TWebKitNavigationResponse = 1;
  WEBKIT_NAVIGATION_RESPONSE_DOWNLOAD: TWebKitNavigationResponse = 2;

type
  TWebKitNetworkError = Integer;
const
  { WebKitNetworkError }
  WEBKIT_NETWORK_ERROR_FAILED: TWebKitNetworkError = 399;
  WEBKIT_NETWORK_ERROR_TRANSPORT: TWebKitNetworkError = 300;
  WEBKIT_NETWORK_ERROR_UNKNOWN_PROTOCOL: TWebKitNetworkError = 301;
  WEBKIT_NETWORK_ERROR_CANCELLED: TWebKitNetworkError = 302;
  WEBKIT_NETWORK_ERROR_FILE_DOES_NOT_EXIST: TWebKitNetworkError = 303;

type
  TWebKitPluginError = Integer;
const
  { WebKitPluginError }
  WEBKIT_PLUGIN_ERROR_FAILED: TWebKitPluginError = 299;
  WEBKIT_PLUGIN_ERROR_CANNOT_FIND_PLUGIN: TWebKitPluginError = 200;
  WEBKIT_PLUGIN_ERROR_CANNOT_LOAD_PLUGIN: TWebKitPluginError = 201;
  WEBKIT_PLUGIN_ERROR_JAVA_UNAVAILABLE: TWebKitPluginError = 202;
  WEBKIT_PLUGIN_ERROR_CONNECTION_CANCELLED: TWebKitPluginError = 203;
  WEBKIT_PLUGIN_ERROR_WILL_HANDLE_LOAD: TWebKitPluginError = 204;

type
  TWebKitPolicyError = Integer;
const
  { WebKitPolicyError }
  WEBKIT_POLICY_ERROR_FAILED: TWebKitPolicyError = 199;
  WEBKIT_POLICY_ERROR_CANNOT_SHOW_MIME_TYPE: TWebKitPolicyError = 100;
  WEBKIT_POLICY_ERROR_CANNOT_SHOW_URL: TWebKitPolicyError = 101;
  WEBKIT_POLICY_ERROR_FRAME_LOAD_INTERRUPTED_BY_POLICY_CHANGE: TWebKitPolicyError = 102;
  WEBKIT_POLICY_ERROR_CANNOT_USE_RESTRICTED_PORT: TWebKitPolicyError = 103;

type
  TWebKitSelectionAffinity = Integer;
const
  { WebKitSelectionAffinity }
  WEBKIT_SELECTION_AFFINITY_UPSTREAM: TWebKitSelectionAffinity = 0;
  WEBKIT_SELECTION_AFFINITY_DOWNSTREAM: TWebKitSelectionAffinity = 1;

type
  TWebKitWebViewViewMode = Integer;
const
  { WebKitWebViewViewMode }
  WEBKIT_WEB_VIEW_VIEW_MODE_WINDOWED: TWebKitWebViewViewMode = 0;
  WEBKIT_WEB_VIEW_VIEW_MODE_FLOATING: TWebKitWebViewViewMode = 1;
  WEBKIT_WEB_VIEW_VIEW_MODE_FULLSCREEN: TWebKitWebViewViewMode = 2;
  WEBKIT_WEB_VIEW_VIEW_MODE_MAXIMIZED: TWebKitWebViewViewMode = 3;
  WEBKIT_WEB_VIEW_VIEW_MODE_MINIMIZED: TWebKitWebViewViewMode = 4;

type
  TWebKitWebNavigationReason = Integer;
const
  { WebKitWebNavigationReason }
  WEBKIT_WEB_NAVIGATION_REASON_LINK_CLICKED: TWebKitWebNavigationReason = 0;
  WEBKIT_WEB_NAVIGATION_REASON_FORM_SUBMITTED: TWebKitWebNavigationReason = 1;
  WEBKIT_WEB_NAVIGATION_REASON_BACK_FORWARD: TWebKitWebNavigationReason = 2;
  WEBKIT_WEB_NAVIGATION_REASON_RELOAD: TWebKitWebNavigationReason = 3;
  WEBKIT_WEB_NAVIGATION_REASON_FORM_RESUBMITTED: TWebKitWebNavigationReason = 4;
  WEBKIT_WEB_NAVIGATION_REASON_OTHER: TWebKitWebNavigationReason = 5;

type
  TWebKitWebViewTargetInfo = Integer;
const
  { WebKitWebViewTargetInfo }
  WEBKIT_WEB_VIEW_TARGET_INFO_HTML: TWebKitWebViewTargetInfo = 0;
  WEBKIT_WEB_VIEW_TARGET_INFO_TEXT: TWebKitWebViewTargetInfo = 1;
  WEBKIT_WEB_VIEW_TARGET_INFO_IMAGE: TWebKitWebViewTargetInfo = 2;
  WEBKIT_WEB_VIEW_TARGET_INFO_URI_LIST: TWebKitWebViewTargetInfo = 3;
  WEBKIT_WEB_VIEW_TARGET_INFO_NETSCAPE_URL: TWebKitWebViewTargetInfo = 4;
type

  PPWebKitCacheModel = ^PWebKitCacheModel;
  PWebKitCacheModel = ^TWebKitCacheModel;

  PPWebKitDOMEventTarget = ^PWebKitDOMEventTarget;
  PWebKitDOMEventTarget = ^TWebKitDOMEventTarget;

  PPWebKitDOMEvent = ^PWebKitDOMEvent;
  PWebKitDOMEvent = ^TWebKitDOMEvent;
  TWebKitDOMEventTarget = object
    function add_event_listener(eventName: Pgchar; handler: TGCallback; bubble: gboolean; userData: gpointer): gboolean; cdecl; inline;
    procedure dispatch_event(event: PWebKitDOMEvent); cdecl; inline;
    function remove_event_listener(eventName: Pgchar; handler: TGCallback; bubble: gboolean): gboolean; cdecl; inline;
  end;

  PPWebKitDOMElement = ^PWebKitDOMElement;
  PWebKitDOMElement = ^TWebKitDOMElement;

  PPWebKitDOMNode = ^PWebKitDOMNode;
  PWebKitDOMNode = ^TWebKitDOMNode;

  PPWebKitDOMObject = ^PWebKitDOMObject;
  PWebKitDOMObject = ^TWebKitDOMObject;
  TWebKitDOMObject = object(TGObject)
    coreObject: gpointer;
    //property core_object: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_core_object  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMNamedNodeMap = ^PWebKitDOMNamedNodeMap;
  PWebKitDOMNamedNodeMap = ^TWebKitDOMNamedNodeMap;

  PPWebKitDOMNodeList = ^PWebKitDOMNodeList;
  PWebKitDOMNodeList = ^TWebKitDOMNodeList;

  PPWebKitDOMDocument = ^PWebKitDOMDocument;
  PWebKitDOMDocument = ^TWebKitDOMDocument;
  TWebKitDOMNode = object(TWebKitDOMObject)
    function append_child(new_child: PWebKitDOMNode): PWebKitDOMNode; cdecl; inline;
    function clone_node(deep: gboolean): PWebKitDOMNode; cdecl; inline;
    function compare_document_position(other: PWebKitDOMNode): gushort; cdecl; inline;
    function dispatch_event(event: PWebKitDOMEvent): gboolean; cdecl; inline;
    function get_attributes: PWebKitDOMNamedNodeMap; cdecl; inline;
    function get_base_uri: Pgchar; cdecl; inline;
    function get_child_nodes: PWebKitDOMNodeList; cdecl; inline;
    function get_first_child: PWebKitDOMNode; cdecl; inline;
    function get_last_child: PWebKitDOMNode; cdecl; inline;
    function get_local_name: Pgchar; cdecl; inline;
    function get_namespace_uri: Pgchar; cdecl; inline;
    function get_next_sibling: PWebKitDOMNode; cdecl; inline;
    function get_node_name: Pgchar; cdecl; inline;
    function get_node_type: gushort; cdecl; inline;
    function get_node_value: Pgchar; cdecl; inline;
    function get_owner_document: PWebKitDOMDocument; cdecl; inline;
    function get_parent_element: PWebKitDOMElement; cdecl; inline;
    function get_parent_node: PWebKitDOMNode; cdecl; inline;
    function get_prefix: Pgchar; cdecl; inline;
    function get_previous_sibling: PWebKitDOMNode; cdecl; inline;
    function get_text_content: Pgchar; cdecl; inline;
    function has_attributes: gboolean; cdecl; inline;
    function has_child_nodes: gboolean; cdecl; inline;
    function insert_before(new_child: PWebKitDOMNode; ref_child: PWebKitDOMNode): PWebKitDOMNode; cdecl; inline;
    function is_default_namespace(namespace_uri: Pgchar): gboolean; cdecl; inline;
    function is_equal_node(other: PWebKitDOMNode): gboolean; cdecl; inline;
    function is_same_node(other: PWebKitDOMNode): gboolean; cdecl; inline;
    function is_supported(feature: Pgchar; version: Pgchar): gboolean; cdecl; inline;
    function lookup_namespace_uri(prefix: Pgchar): Pgchar; cdecl; inline;
    function lookup_prefix(namespace_uri: Pgchar): Pgchar; cdecl; inline;
    procedure normalize; cdecl; inline;
    function remove_child(old_child: PWebKitDOMNode): PWebKitDOMNode; cdecl; inline;
    function replace_child(new_child: PWebKitDOMNode; old_child: PWebKitDOMNode): PWebKitDOMNode; cdecl; inline;
    procedure set_node_value(value: Pgchar); cdecl; inline;
    procedure set_prefix(value: Pgchar); cdecl; inline;
    procedure set_text_content(value: Pgchar); cdecl; inline;
    property attributes:  PWebKitDOMNamedNodeMap read get_attributes ;
    property base_uri:  Pgchar read get_base_uri ;
    property child_nodes:  PWebKitDOMNodeList read get_child_nodes ;
    property first_child:  PWebKitDOMNode read get_first_child ;
    property last_child:  PWebKitDOMNode read get_last_child ;
    property local_name:  Pgchar read get_local_name ;
    property namespace_uri:  Pgchar read get_namespace_uri ;
    property next_sibling:  PWebKitDOMNode read get_next_sibling ;
    property node_name:  Pgchar read get_node_name ;
    property node_type:  gushort read get_node_type ;
    property node_value:  Pgchar read get_node_value  { property is writeable but setter not declared } ;
    property owner_document:  PWebKitDOMDocument read get_owner_document ;
    property parent_element:  PWebKitDOMElement read get_parent_element ;
    property parent_node:  PWebKitDOMNode read get_parent_node ;
    property prefix:  Pgchar read get_prefix  { property is writeable but setter not declared } ;
    property previous_sibling:  PWebKitDOMNode read get_previous_sibling ;
    property text_content:  Pgchar read get_text_content  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMAttr = ^PWebKitDOMAttr;
  PWebKitDOMAttr = ^TWebKitDOMAttr;

  PPWebKitDOMCSSStyleDeclaration = ^PWebKitDOMCSSStyleDeclaration;
  PWebKitDOMCSSStyleDeclaration = ^TWebKitDOMCSSStyleDeclaration;

  PPWebKitDOMWebKitAnimationList = ^PWebKitDOMWebKitAnimationList;
  PWebKitDOMWebKitAnimationList = ^TWebKitDOMWebKitAnimationList;
  TWebKitDOMElement = object(TWebKitDOMNode)
    procedure blur; cdecl; inline;
    function contains(element: PWebKitDOMElement): gboolean; cdecl; inline;
    procedure focus; cdecl; inline;
    function get_attribute(name: Pgchar): Pgchar; cdecl; inline;
    function get_attribute_node(name: Pgchar): PWebKitDOMAttr; cdecl; inline;
    function get_attribute_node_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMAttr; cdecl; inline;
    function get_attribute_ns(namespace_uri: Pgchar; local_name: Pgchar): Pgchar; cdecl; inline;
    function get_child_element_count: gulong; cdecl; inline;
    function get_client_height: glong; cdecl; inline;
    function get_client_left: glong; cdecl; inline;
    function get_client_top: glong; cdecl; inline;
    function get_client_width: glong; cdecl; inline;
    function get_elements_by_class_name(name: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    function get_elements_by_tag_name(name: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    function get_elements_by_tag_name_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    function get_first_element_child: PWebKitDOMElement; cdecl; inline;
    function get_last_element_child: PWebKitDOMElement; cdecl; inline;
    function get_next_element_sibling: PWebKitDOMElement; cdecl; inline;
    function get_offset_height: glong; cdecl; inline;
    function get_offset_left: glong; cdecl; inline;
    function get_offset_parent: PWebKitDOMElement; cdecl; inline;
    function get_offset_top: glong; cdecl; inline;
    function get_offset_width: glong; cdecl; inline;
    function get_previous_element_sibling: PWebKitDOMElement; cdecl; inline;
    function get_scroll_height: glong; cdecl; inline;
    function get_scroll_left: glong; cdecl; inline;
    function get_scroll_top: glong; cdecl; inline;
    function get_scroll_width: glong; cdecl; inline;
    function get_style: PWebKitDOMCSSStyleDeclaration; cdecl; inline;
    function get_tag_name: Pgchar; cdecl; inline;
    function has_attribute(name: Pgchar): gboolean; cdecl; inline;
    function has_attribute_ns(namespace_uri: Pgchar; local_name: Pgchar): gboolean; cdecl; inline;
    function query_selector(selectors: Pgchar): PWebKitDOMElement; cdecl; inline;
    function query_selector_all(selectors: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    procedure remove_attribute(name: Pgchar); cdecl; inline;
    function remove_attribute_node(old_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl; inline;
    procedure remove_attribute_ns(namespace_uri: Pgchar; local_name: Pgchar); cdecl; inline;
    procedure scroll_by_lines(lines: glong); cdecl; inline;
    procedure scroll_by_pages(pages: glong); cdecl; inline;
    procedure scroll_into_view(align_with_top: gboolean); cdecl; inline;
    procedure scroll_into_view_if_needed(center_if_needed: gboolean); cdecl; inline;
    procedure set_attribute(name: Pgchar; value: Pgchar); cdecl; inline;
    function set_attribute_node(new_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl; inline;
    function set_attribute_node_ns(new_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl; inline;
    procedure set_attribute_ns(namespace_uri: Pgchar; qualified_name: Pgchar; value: Pgchar); cdecl; inline;
    procedure set_scroll_left(value: glong); cdecl; inline;
    procedure set_scroll_top(value: glong); cdecl; inline;
    function webkit_get_animations: PWebKitDOMWebKitAnimationList; cdecl; inline;
    function webkit_matches_selector(selectors: Pgchar): gboolean; cdecl; inline;
    procedure webkit_request_full_screen(flags: gushort); cdecl; inline;
    property child_element_count:  gulong read get_child_element_count ;
    property client_height:  glong read get_client_height ;
    property client_left:  glong read get_client_left ;
    property client_top:  glong read get_client_top ;
    property client_width:  glong read get_client_width ;
    property first_element_child:  PWebKitDOMElement read get_first_element_child ;
    property last_element_child:  PWebKitDOMElement read get_last_element_child ;
    property next_element_sibling:  PWebKitDOMElement read get_next_element_sibling ;
    property offset_height:  glong read get_offset_height ;
    property offset_left:  glong read get_offset_left ;
    property offset_parent:  PWebKitDOMElement read get_offset_parent ;
    property offset_top:  glong read get_offset_top ;
    property offset_width:  glong read get_offset_width ;
    property previous_element_sibling:  PWebKitDOMElement read get_previous_element_sibling ;
    property scroll_height:  glong read get_scroll_height ;
    property scroll_left:  glong read get_scroll_left  { property is writeable but setter not declared } ;
    property scroll_top:  glong read get_scroll_top  { property is writeable but setter not declared } ;
    property scroll_width:  glong read get_scroll_width ;
    property style:  PWebKitDOMCSSStyleDeclaration read get_style ;
    property tag_name:  Pgchar read get_tag_name ;
  end;
  TWebKitDOMAttr = object(TWebKitDOMNode)
    function get_is_id: gboolean; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_owner_element: PWebKitDOMElement; cdecl; inline;
    function get_specified: gboolean; cdecl; inline;
    function get_value: Pgchar; cdecl; inline;
    procedure set_value(value: Pgchar); cdecl; inline;
    property is_id:  gboolean read get_is_id ;
    property name:  Pgchar read get_name ;
    property owner_element:  PWebKitDOMElement read get_owner_element ;
    property specified:  gboolean read get_specified ;
    property value:  Pgchar read get_value  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMNodeClass = ^PWebKitDOMNodeClass;
  PWebKitDOMNodeClass = ^TWebKitDOMNodeClass;

  PPWebKitDOMObjectClass = ^PWebKitDOMObjectClass;
  PWebKitDOMObjectClass = ^TWebKitDOMObjectClass;
  TWebKitDOMObjectClass = object
    parentClass: TGObjectClass;
  end;
  TWebKitDOMNodeClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMAttrClass = ^PWebKitDOMAttrClass;
  PWebKitDOMAttrClass = ^TWebKitDOMAttrClass;
  TWebKitDOMAttrClass = object
    parent_class: TWebKitDOMNodeClass;
  end;

  PPWebKitDOMBarInfo = ^PWebKitDOMBarInfo;
  PWebKitDOMBarInfo = ^TWebKitDOMBarInfo;
  TWebKitDOMBarInfo = object(TWebKitDOMObject)
    function get_visible: gboolean; cdecl; inline;
    property visible:  gboolean read get_visible ;
  end;

  PPWebKitDOMBarInfoClass = ^PWebKitDOMBarInfoClass;
  PWebKitDOMBarInfoClass = ^TWebKitDOMBarInfoClass;
  TWebKitDOMBarInfoClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMBlob = ^PWebKitDOMBlob;
  PWebKitDOMBlob = ^TWebKitDOMBlob;
  TWebKitDOMBlob = object(TWebKitDOMObject)
    function get_size: guint64; cdecl; inline;
    function slice(start: gint64; length: gint64; content_type: Pgchar): PWebKitDOMBlob; cdecl; inline;
    property size:  guint64 read get_size ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
  end;

  PPWebKitDOMBlobClass = ^PWebKitDOMBlobClass;
  PWebKitDOMBlobClass = ^TWebKitDOMBlobClass;
  TWebKitDOMBlobClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMText = ^PWebKitDOMText;
  PWebKitDOMText = ^TWebKitDOMText;

  PPWebKitDOMCharacterData = ^PWebKitDOMCharacterData;
  PWebKitDOMCharacterData = ^TWebKitDOMCharacterData;
  TWebKitDOMCharacterData = object(TWebKitDOMNode)
    procedure append_data(data: Pgchar); cdecl; inline;
    procedure delete_data(offset: gulong; length: gulong); cdecl; inline;
    function get_data: Pgchar; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    procedure insert_data(offset: gulong; data: Pgchar); cdecl; inline;
    procedure replace_data(offset: gulong; length: gulong; data: Pgchar); cdecl; inline;
    procedure set_data(value: Pgchar); cdecl; inline;
    function substring_data(offset: gulong; length: gulong): Pgchar; cdecl; inline;
    property data:  Pgchar read get_data  { property is writeable but setter not declared } ;
    property length:  gulong read get_length ;
  end;
  TWebKitDOMText = object(TWebKitDOMCharacterData)
    function get_whole_text: Pgchar; cdecl; inline;
    function replace_whole_text(content: Pgchar): PWebKitDOMText; cdecl; inline;
    function split_text(offset: gulong): PWebKitDOMText; cdecl; inline;
    property whole_text:  Pgchar read get_whole_text ;
  end;

  PPWebKitDOMCDATASection = ^PWebKitDOMCDATASection;
  PWebKitDOMCDATASection = ^TWebKitDOMCDATASection;
  TWebKitDOMCDATASection = object(TWebKitDOMText)
  end;

  PPWebKitDOMTextClass = ^PWebKitDOMTextClass;
  PWebKitDOMTextClass = ^TWebKitDOMTextClass;

  PPWebKitDOMCharacterDataClass = ^PWebKitDOMCharacterDataClass;
  PWebKitDOMCharacterDataClass = ^TWebKitDOMCharacterDataClass;
  TWebKitDOMCharacterDataClass = object
    parent_class: TWebKitDOMNodeClass;
  end;
  TWebKitDOMTextClass = object
    parent_class: TWebKitDOMCharacterDataClass;
  end;

  PPWebKitDOMCDATASectionClass = ^PWebKitDOMCDATASectionClass;
  PWebKitDOMCDATASectionClass = ^TWebKitDOMCDATASectionClass;
  TWebKitDOMCDATASectionClass = object
    parent_class: TWebKitDOMTextClass;
  end;

  PPWebKitDOMCSSRule = ^PWebKitDOMCSSRule;
  PWebKitDOMCSSRule = ^TWebKitDOMCSSRule;

  PPWebKitDOMCSSStyleSheet = ^PWebKitDOMCSSStyleSheet;
  PWebKitDOMCSSStyleSheet = ^TWebKitDOMCSSStyleSheet;
  TWebKitDOMCSSRule = object(TWebKitDOMObject)
    function get_css_text: Pgchar; cdecl; inline;
    function get_parent_rule: PWebKitDOMCSSRule; cdecl; inline;
    function get_parent_style_sheet: PWebKitDOMCSSStyleSheet; cdecl; inline;
    procedure set_css_text(value: Pgchar); cdecl; inline;
    property css_text:  Pgchar read get_css_text  { property is writeable but setter not declared } ;
    property parent_rule:  PWebKitDOMCSSRule read get_parent_rule ;
    property parent_style_sheet:  PWebKitDOMCSSStyleSheet read get_parent_style_sheet ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
  end;

  PPWebKitDOMStyleSheet = ^PWebKitDOMStyleSheet;
  PWebKitDOMStyleSheet = ^TWebKitDOMStyleSheet;

  PPWebKitDOMMediaList = ^PWebKitDOMMediaList;
  PWebKitDOMMediaList = ^TWebKitDOMMediaList;
  TWebKitDOMStyleSheet = object(TWebKitDOMObject)
    function get_disabled: gboolean; cdecl; inline;
    function get_href: Pgchar; cdecl; inline;
    function get_media: PWebKitDOMMediaList; cdecl; inline;
    function get_owner_node: PWebKitDOMNode; cdecl; inline;
    function get_parent_style_sheet: PWebKitDOMStyleSheet; cdecl; inline;
    function get_title: Pgchar; cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property href:  Pgchar read get_href ;
    property media:  PWebKitDOMMediaList read get_media ;
    property owner_node:  PWebKitDOMNode read get_owner_node ;
    property parent_style_sheet:  PWebKitDOMStyleSheet read get_parent_style_sheet ;
    property title:  Pgchar read get_title ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
  end;

  PPWebKitDOMCSSRuleList = ^PWebKitDOMCSSRuleList;
  PWebKitDOMCSSRuleList = ^TWebKitDOMCSSRuleList;
  TWebKitDOMCSSStyleSheet = object(TWebKitDOMStyleSheet)
    function add_rule(selector: Pgchar; style: Pgchar; index: gulong): glong; cdecl; inline;
    procedure delete_rule(index: gulong); cdecl; inline;
    function get_css_rules: PWebKitDOMCSSRuleList; cdecl; inline;
    function get_owner_rule: PWebKitDOMCSSRule; cdecl; inline;
    function get_rules: PWebKitDOMCSSRuleList; cdecl; inline;
    function insert_rule(rule: Pgchar; index: gulong): gulong; cdecl; inline;
    procedure remove_rule(index: gulong); cdecl; inline;
    property css_rules:  PWebKitDOMCSSRuleList read get_css_rules ;
    property owner_rule:  PWebKitDOMCSSRule read get_owner_rule ;
    property rules:  PWebKitDOMCSSRuleList read get_rules ;
  end;

  PPWebKitDOMCSSRuleClass = ^PWebKitDOMCSSRuleClass;
  PWebKitDOMCSSRuleClass = ^TWebKitDOMCSSRuleClass;
  TWebKitDOMCSSRuleClass = object
    parent_class: TWebKitDOMObjectClass;
  end;
  TWebKitDOMCSSRuleList = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): PWebKitDOMCSSRule; cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMCSSRuleListClass = ^PWebKitDOMCSSRuleListClass;
  PWebKitDOMCSSRuleListClass = ^TWebKitDOMCSSRuleListClass;
  TWebKitDOMCSSRuleListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMCSSValue = ^PWebKitDOMCSSValue;
  PWebKitDOMCSSValue = ^TWebKitDOMCSSValue;
  TWebKitDOMCSSValue = object(TWebKitDOMObject)
    function get_css_text: Pgchar; cdecl; inline;
    function get_css_value_type: gushort; cdecl; inline;
    procedure set_css_text(value: Pgchar); cdecl; inline;
    property css_text:  Pgchar read get_css_text  { property is writeable but setter not declared } ;
    property css_value_type:  gushort read get_css_value_type ;
  end;
  TWebKitDOMCSSStyleDeclaration = object(TWebKitDOMObject)
    function get_css_text: Pgchar; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    function get_parent_rule: PWebKitDOMCSSRule; cdecl; inline;
    function get_property_css_value(property_name: Pgchar): PWebKitDOMCSSValue; cdecl; inline;
    function get_property_priority(property_name: Pgchar): Pgchar; cdecl; inline;
    function get_property_shorthand(property_name: Pgchar): Pgchar; cdecl; inline;
    function get_property_value(property_name: Pgchar): Pgchar; cdecl; inline;
    function is_property_implicit(property_name: Pgchar): gboolean; cdecl; inline;
    function item(index: gulong): Pgchar; cdecl; inline;
    function remove_property(property_name: Pgchar): Pgchar; cdecl; inline;
    procedure set_css_text(value: Pgchar); cdecl; inline;
    procedure set_property(property_name: Pgchar; value: Pgchar; priority: Pgchar); cdecl; inline;
    property css_text:  Pgchar read get_css_text  { property is writeable but setter not declared } ;
    property length:  gulong read get_length ;
    property parent_rule:  PWebKitDOMCSSRule read get_parent_rule ;
  end;

  PPWebKitDOMCSSStyleDeclarationClass = ^PWebKitDOMCSSStyleDeclarationClass;
  PWebKitDOMCSSStyleDeclarationClass = ^TWebKitDOMCSSStyleDeclarationClass;
  TWebKitDOMCSSStyleDeclarationClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMStyleSheetClass = ^PWebKitDOMStyleSheetClass;
  PWebKitDOMStyleSheetClass = ^TWebKitDOMStyleSheetClass;
  TWebKitDOMStyleSheetClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMCSSStyleSheetClass = ^PWebKitDOMCSSStyleSheetClass;
  PWebKitDOMCSSStyleSheetClass = ^TWebKitDOMCSSStyleSheetClass;
  TWebKitDOMCSSStyleSheetClass = object
    parent_class: TWebKitDOMStyleSheetClass;
  end;

  PPWebKitDOMCSSValueClass = ^PWebKitDOMCSSValueClass;
  PWebKitDOMCSSValueClass = ^TWebKitDOMCSSValueClass;
  TWebKitDOMCSSValueClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMComment = ^PWebKitDOMComment;
  PWebKitDOMComment = ^TWebKitDOMComment;
  TWebKitDOMComment = object(TWebKitDOMCharacterData)
  end;

  PPWebKitDOMCommentClass = ^PWebKitDOMCommentClass;
  PWebKitDOMCommentClass = ^TWebKitDOMCommentClass;
  TWebKitDOMCommentClass = object
    parent_class: TWebKitDOMCharacterDataClass;
  end;

  PPWebKitDOMMemoryInfo = ^PWebKitDOMMemoryInfo;
  PWebKitDOMMemoryInfo = ^TWebKitDOMMemoryInfo;
  TWebKitDOMMemoryInfo = object(TWebKitDOMObject)
    function get_js_heap_size_limit: gulong; cdecl; inline;
    function get_total_js_heap_size: gulong; cdecl; inline;
    function get_used_js_heap_size: gulong; cdecl; inline;
    property js_heap_size_limit:  gulong read get_js_heap_size_limit ;
    property total_js_heap_size:  gulong read get_total_js_heap_size ;
    property used_js_heap_size:  gulong read get_used_js_heap_size ;
  end;

  PPWebKitDOMConsole = ^PWebKitDOMConsole;
  PWebKitDOMConsole = ^TWebKitDOMConsole;
  TWebKitDOMConsole = object(TWebKitDOMObject)
    function get_memory: PWebKitDOMMemoryInfo; cdecl; inline;
    procedure group_end; cdecl; inline;
    procedure time(title: Pgchar); cdecl; inline;
    property memory:  PWebKitDOMMemoryInfo read get_memory ;
  end;

  PPWebKitDOMConsoleClass = ^PWebKitDOMConsoleClass;
  PWebKitDOMConsoleClass = ^TWebKitDOMConsoleClass;
  TWebKitDOMConsoleClass = object
    parent_class: TWebKitDOMObjectClass;
  end;
  TWebKitDOMEvent = object(TWebKitDOMObject)
    function get_bubbles: gboolean; cdecl; inline;
    function get_cancel_bubble: gboolean; cdecl; inline;
    function get_cancelable: gboolean; cdecl; inline;
    function get_current_target: PWebKitDOMEventTarget; cdecl; inline;
    function get_default_prevented: gboolean; cdecl; inline;
    function get_event_phase: gushort; cdecl; inline;
    function get_return_value: gboolean; cdecl; inline;
    function get_src_element: PWebKitDOMEventTarget; cdecl; inline;
    function get_target: PWebKitDOMEventTarget; cdecl; inline;
    function get_time_stamp: guint32; cdecl; inline;
    procedure init_event(event_type_arg: Pgchar; can_bubble_arg: gboolean; cancelable_arg: gboolean); cdecl; inline;
    procedure prevent_default; cdecl; inline;
    procedure set_cancel_bubble(value: gboolean); cdecl; inline;
    procedure set_return_value(value: gboolean); cdecl; inline;
    procedure stop_immediate_propagation; cdecl; inline;
    procedure stop_propagation; cdecl; inline;
    property bubbles:  gboolean read get_bubbles ;
    property cancel_bubble:  gboolean read get_cancel_bubble  { property is writeable but setter not declared } ;
    property cancelable:  gboolean read get_cancelable ;
    property current_target:  PWebKitDOMEventTarget read get_current_target ;
    property default_prevented:  gboolean read get_default_prevented ;
    property event_phase:  gushort read get_event_phase ;
    property return_value:  gboolean read get_return_value  { property is writeable but setter not declared } ;
    property src_element:  PWebKitDOMEventTarget read get_src_element ;
    property target:  PWebKitDOMEventTarget read get_target ;
    property time_stamp:  guint32 read get_time_stamp ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
  end;

  PPWebKitDOMDOMApplicationCache = ^PWebKitDOMDOMApplicationCache;
  PWebKitDOMDOMApplicationCache = ^TWebKitDOMDOMApplicationCache;
  TWebKitDOMDOMApplicationCache = object(TWebKitDOMObject)
    function dispatch_event(evt: PWebKitDOMEvent): gboolean; cdecl; inline;
    function get_status: gushort; cdecl; inline;
    procedure swap_cache; cdecl; inline;
    procedure update; cdecl; inline;
    property status:  gushort read get_status ;
  end;

  PPWebKitDOMDOMApplicationCacheClass = ^PWebKitDOMDOMApplicationCacheClass;
  PWebKitDOMDOMApplicationCacheClass = ^TWebKitDOMDOMApplicationCacheClass;
  TWebKitDOMDOMApplicationCacheClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMRange = ^PWebKitDOMRange;
  PWebKitDOMRange = ^TWebKitDOMRange;

  PPWebKitDOMDocumentFragment = ^PWebKitDOMDocumentFragment;
  PWebKitDOMDocumentFragment = ^TWebKitDOMDocumentFragment;

  PPWebKitDOMEntityReference = ^PWebKitDOMEntityReference;
  PWebKitDOMEntityReference = ^TWebKitDOMEntityReference;

  PPWebKitDOMXPathExpression = ^PWebKitDOMXPathExpression;
  PWebKitDOMXPathExpression = ^TWebKitDOMXPathExpression;

  PPWebKitDOMXPathNSResolver = ^PWebKitDOMXPathNSResolver;
  PWebKitDOMXPathNSResolver = ^TWebKitDOMXPathNSResolver;

  PPWebKitDOMNodeIterator = ^PWebKitDOMNodeIterator;
  PWebKitDOMNodeIterator = ^TWebKitDOMNodeIterator;

  PPWebKitDOMNodeFilter = ^PWebKitDOMNodeFilter;
  PWebKitDOMNodeFilter = ^TWebKitDOMNodeFilter;

  PPWebKitDOMProcessingInstruction = ^PWebKitDOMProcessingInstruction;
  PWebKitDOMProcessingInstruction = ^TWebKitDOMProcessingInstruction;

  PPWebKitDOMTreeWalker = ^PWebKitDOMTreeWalker;
  PWebKitDOMTreeWalker = ^TWebKitDOMTreeWalker;

  PPWebKitDOMXPathResult = ^PWebKitDOMXPathResult;
  PWebKitDOMXPathResult = ^TWebKitDOMXPathResult;

  PPWebKitDOMHTMLCollection = ^PWebKitDOMHTMLCollection;
  PWebKitDOMHTMLCollection = ^TWebKitDOMHTMLCollection;

  PPWebKitDOMHTMLElement = ^PWebKitDOMHTMLElement;
  PWebKitDOMHTMLElement = ^TWebKitDOMHTMLElement;

  PPWebKitDOMDOMWindow = ^PWebKitDOMDOMWindow;
  PWebKitDOMDOMWindow = ^TWebKitDOMDOMWindow;

  PPWebKitDOMDocumentType = ^PWebKitDOMDocumentType;
  PWebKitDOMDocumentType = ^TWebKitDOMDocumentType;

  PPWebKitDOMHTMLHeadElement = ^PWebKitDOMHTMLHeadElement;
  PWebKitDOMHTMLHeadElement = ^TWebKitDOMHTMLHeadElement;

  PPWebKitDOMDOMImplementation = ^PWebKitDOMDOMImplementation;
  PWebKitDOMDOMImplementation = ^TWebKitDOMDOMImplementation;

  PPWebKitDOMStyleSheetList = ^PWebKitDOMStyleSheetList;
  PWebKitDOMStyleSheetList = ^TWebKitDOMStyleSheetList;
  TWebKitDOMDocument = object(TWebKitDOMNode)
    function adopt_node(source: PWebKitDOMNode): PWebKitDOMNode; cdecl; inline;
    function caret_range_from_point(x: glong; y: glong): PWebKitDOMRange; cdecl; inline;
    function create_attribute(name: Pgchar): PWebKitDOMAttr; cdecl; inline;
    function create_attribute_ns(namespace_uri: Pgchar; qualified_name: Pgchar): PWebKitDOMAttr; cdecl; inline;
    function create_cdata_section(data: Pgchar): PWebKitDOMCDATASection; cdecl; inline;
    function create_comment(data: Pgchar): PWebKitDOMComment; cdecl; inline;
    function create_css_style_declaration: PWebKitDOMCSSStyleDeclaration; cdecl; inline;
    function create_document_fragment: PWebKitDOMDocumentFragment; cdecl; inline;
    function create_element(tag_name: Pgchar): PWebKitDOMElement; cdecl; inline;
    function create_element_ns(namespace_uri: Pgchar; qualified_name: Pgchar): PWebKitDOMElement; cdecl; inline;
    function create_entity_reference(name: Pgchar): PWebKitDOMEntityReference; cdecl; inline;
    function create_event(event_type: Pgchar): PWebKitDOMEvent; cdecl; inline;
    function create_expression(expression: Pgchar; resolver: PWebKitDOMXPathNSResolver): PWebKitDOMXPathExpression; cdecl; inline;
    function create_node_iterator(root: PWebKitDOMNode; what_to_show: gulong; filter: PWebKitDOMNodeFilter; expand_entity_references: gboolean): PWebKitDOMNodeIterator; cdecl; inline;
    function create_ns_resolver(node_resolver: PWebKitDOMNode): PWebKitDOMXPathNSResolver; cdecl; inline;
    function create_processing_instruction(target: Pgchar; data: Pgchar): PWebKitDOMProcessingInstruction; cdecl; inline;
    function create_range: PWebKitDOMRange; cdecl; inline;
    function create_text_node(data: Pgchar): PWebKitDOMText; cdecl; inline;
    function create_tree_walker(root: PWebKitDOMNode; what_to_show: gulong; filter: PWebKitDOMNodeFilter; expand_entity_references: gboolean): PWebKitDOMTreeWalker; cdecl; inline;
    function element_from_point(x: glong; y: glong): PWebKitDOMElement; cdecl; inline;
    function evaluate(expression: Pgchar; context_node: PWebKitDOMNode; resolver: PWebKitDOMXPathNSResolver; type_: gushort; in_result: PWebKitDOMXPathResult): PWebKitDOMXPathResult; cdecl; inline;
    function exec_command(command: Pgchar; user_interface: gboolean; value: Pgchar): gboolean; cdecl; inline;
    function get_anchors: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_applets: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_body: PWebKitDOMHTMLElement; cdecl; inline;
    function get_character_set: Pgchar; cdecl; inline;
    function get_charset: Pgchar; cdecl; inline;
    function get_compat_mode: Pgchar; cdecl; inline;
    function get_cookie: Pgchar; cdecl; inline;
    function get_default_charset: Pgchar; cdecl; inline;
    function get_default_view: PWebKitDOMDOMWindow; cdecl; inline;
    function get_doctype: PWebKitDOMDocumentType; cdecl; inline;
    function get_document_element: PWebKitDOMElement; cdecl; inline;
    function get_document_uri: Pgchar; cdecl; inline;
    function get_domain: Pgchar; cdecl; inline;
    function get_element_by_id(element_id: Pgchar): PWebKitDOMElement; cdecl; inline;
    function get_elements_by_class_name(tagname: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    function get_elements_by_name(element_name: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    function get_elements_by_tag_name(tagname: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    function get_elements_by_tag_name_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    function get_forms: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_head: PWebKitDOMHTMLHeadElement; cdecl; inline;
    function get_images: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_implementation: PWebKitDOMDOMImplementation; cdecl; inline;
    function get_input_encoding: Pgchar; cdecl; inline;
    function get_last_modified: Pgchar; cdecl; inline;
    function get_links: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_override_style(element: PWebKitDOMElement; pseudo_element: Pgchar): PWebKitDOMCSSStyleDeclaration; cdecl; inline;
    function get_preferred_stylesheet_set: Pgchar; cdecl; inline;
    function get_ready_state: Pgchar; cdecl; inline;
    function get_referrer: Pgchar; cdecl; inline;
    function get_selected_stylesheet_set: Pgchar; cdecl; inline;
    function get_style_sheets: PWebKitDOMStyleSheetList; cdecl; inline;
    function get_title: Pgchar; cdecl; inline;
    function get_webkit_current_full_screen_element: PWebKitDOMElement; cdecl; inline;
    function get_webkit_full_screen_keyboard_input_allowed: gboolean; cdecl; inline;
    function get_webkit_is_full_screen: gboolean; cdecl; inline;
    function get_xml_encoding: Pgchar; cdecl; inline;
    function get_xml_standalone: gboolean; cdecl; inline;
    function get_xml_version: Pgchar; cdecl; inline;
    function import_node(imported_node: PWebKitDOMNode; deep: gboolean): PWebKitDOMNode; cdecl; inline;
    function query_command_enabled(command: Pgchar): gboolean; cdecl; inline;
    function query_command_indeterm(command: Pgchar): gboolean; cdecl; inline;
    function query_command_state(command: Pgchar): gboolean; cdecl; inline;
    function query_command_supported(command: Pgchar): gboolean; cdecl; inline;
    function query_command_value(command: Pgchar): Pgchar; cdecl; inline;
    function query_selector(selectors: Pgchar): PWebKitDOMElement; cdecl; inline;
    function query_selector_all(selectors: Pgchar): PWebKitDOMNodeList; cdecl; inline;
    procedure set_body(value: PWebKitDOMHTMLElement); cdecl; inline;
    procedure set_charset(value: Pgchar); cdecl; inline;
    procedure set_cookie(value: Pgchar); cdecl; inline;
    procedure set_document_uri(value: Pgchar); cdecl; inline;
    procedure set_selected_stylesheet_set(value: Pgchar); cdecl; inline;
    procedure set_title(value: Pgchar); cdecl; inline;
    procedure set_xml_standalone(value: gboolean); cdecl; inline;
    procedure set_xml_version(value: Pgchar); cdecl; inline;
    procedure webkit_cancel_full_screen; cdecl; inline;
    property anchors:  PWebKitDOMHTMLCollection read get_anchors ;
    property applets:  PWebKitDOMHTMLCollection read get_applets ;
    property body:  PWebKitDOMHTMLElement read get_body  { property is writeable but setter not declared } ;
    property character_set:  Pgchar read get_character_set ;
    property charset:  Pgchar read get_charset  { property is writeable but setter not declared } ;
    property compat_mode:  Pgchar read get_compat_mode ;
    property cookie:  Pgchar read get_cookie  { property is writeable but setter not declared } ;
    property default_charset:  Pgchar read get_default_charset ;
    property default_view:  PWebKitDOMDOMWindow read get_default_view ;
    property doctype:  PWebKitDOMDocumentType read get_doctype ;
    property document_element:  PWebKitDOMElement read get_document_element ;
    property document_uri:  Pgchar read get_document_uri  { property is writeable but setter not declared } ;
    property domain:  Pgchar read get_domain ;
    property forms:  PWebKitDOMHTMLCollection read get_forms ;
    property head:  PWebKitDOMHTMLHeadElement read get_head ;
    property images:  PWebKitDOMHTMLCollection read get_images ;
    property implementation_:  PWebKitDOMDOMImplementation read get_implementation ;
    property input_encoding:  Pgchar read get_input_encoding ;
    property last_modified:  Pgchar read get_last_modified ;
    property links:  PWebKitDOMHTMLCollection read get_links ;
    property preferred_stylesheet_set:  Pgchar read get_preferred_stylesheet_set ;
    property ready_state:  Pgchar read get_ready_state ;
    property referrer:  Pgchar read get_referrer ;
    property selected_stylesheet_set:  Pgchar read get_selected_stylesheet_set  { property is writeable but setter not declared } ;
    property style_sheets:  PWebKitDOMStyleSheetList read get_style_sheets ;
    property title:  Pgchar read get_title  { property is writeable but setter not declared } ;
    //property url: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_url ;
    property webkit_current_full_screen_element:  PWebKitDOMElement read get_webkit_current_full_screen_element ;
    property webkit_full_screen_keyboard_input_allowed:  gboolean read get_webkit_full_screen_keyboard_input_allowed ;
    property webkit_is_full_screen:  gboolean read get_webkit_is_full_screen ;
    property xml_encoding:  Pgchar read get_xml_encoding ;
    property xml_standalone:  gboolean read get_xml_standalone  { property is writeable but setter not declared } ;
    property xml_version:  Pgchar read get_xml_version  { property is writeable but setter not declared } ;
  end;
  TWebKitDOMDocumentType = object(TWebKitDOMNode)
    function get_entities: PWebKitDOMNamedNodeMap; cdecl; inline;
    function get_internal_subset: Pgchar; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_notations: PWebKitDOMNamedNodeMap; cdecl; inline;
    function get_public_id: Pgchar; cdecl; inline;
    function get_system_id: Pgchar; cdecl; inline;
    property entities:  PWebKitDOMNamedNodeMap read get_entities ;
    property internal_subset:  Pgchar read get_internal_subset ;
    property name:  Pgchar read get_name ;
    property notations:  PWebKitDOMNamedNodeMap read get_notations ;
    property public_id:  Pgchar read get_public_id ;
    property system_id:  Pgchar read get_system_id ;
  end;

  PPWebKitDOMHTMLDocument = ^PWebKitDOMHTMLDocument;
  PWebKitDOMHTMLDocument = ^TWebKitDOMHTMLDocument;
  TWebKitDOMHTMLDocument = object(TWebKitDOMDocument)
    procedure capture_events; cdecl; inline;
    procedure clear; cdecl; inline;
    procedure close; cdecl; inline;
    function get_active_element: PWebKitDOMElement; cdecl; inline;
    function get_alink_color: Pgchar; cdecl; inline;
    function get_bg_color: Pgchar; cdecl; inline;
    function get_compat_mode: Pgchar; cdecl; inline;
    function get_design_mode: Pgchar; cdecl; inline;
    function get_dir: Pgchar; cdecl; inline;
    function get_embeds: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_fg_color: Pgchar; cdecl; inline;
    function get_height: glong; cdecl; inline;
    function get_link_color: Pgchar; cdecl; inline;
    function get_plugins: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_scripts: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_vlink_color: Pgchar; cdecl; inline;
    function get_width: glong; cdecl; inline;
    function has_focus: gboolean; cdecl; inline;
    procedure release_events; cdecl; inline;
    procedure set_alink_color(value: Pgchar); cdecl; inline;
    procedure set_bg_color(value: Pgchar); cdecl; inline;
    procedure set_design_mode(value: Pgchar); cdecl; inline;
    procedure set_dir(value: Pgchar); cdecl; inline;
    procedure set_fg_color(value: Pgchar); cdecl; inline;
    procedure set_link_color(value: Pgchar); cdecl; inline;
    procedure set_vlink_color(value: Pgchar); cdecl; inline;
    property active_element:  PWebKitDOMElement read get_active_element ;
    property alink_color:  Pgchar read get_alink_color  { property is writeable but setter not declared } ;
    property bg_color:  Pgchar read get_bg_color  { property is writeable but setter not declared } ;
    property compat_mode1:  Pgchar read get_compat_mode ;
    property design_mode:  Pgchar read get_design_mode  { property is writeable but setter not declared } ;
    property dir:  Pgchar read get_dir  { property is writeable but setter not declared } ;
    property embeds:  PWebKitDOMHTMLCollection read get_embeds ;
    property fg_color:  Pgchar read get_fg_color  { property is writeable but setter not declared } ;
    property height:  glong read get_height ;
    property link_color:  Pgchar read get_link_color  { property is writeable but setter not declared } ;
    property plugins:  PWebKitDOMHTMLCollection read get_plugins ;
    property scripts:  PWebKitDOMHTMLCollection read get_scripts ;
    property vlink_color:  Pgchar read get_vlink_color  { property is writeable but setter not declared } ;
    property width:  glong read get_width ;
  end;
  TWebKitDOMDOMImplementation = object(TWebKitDOMObject)
    function create_css_style_sheet(title: Pgchar; media: Pgchar): PWebKitDOMCSSStyleSheet; cdecl; inline;
    function create_document(namespace_uri: Pgchar; qualified_name: Pgchar; doctype: PWebKitDOMDocumentType): PWebKitDOMDocument; cdecl; inline;
    function create_document_type(qualified_name: Pgchar; public_id: Pgchar; system_id: Pgchar): PWebKitDOMDocumentType; cdecl; inline;
    function create_html_document(title: Pgchar): PWebKitDOMHTMLDocument; cdecl; inline;
    function has_feature(feature: Pgchar; version: Pgchar): gboolean; cdecl; inline;
  end;

  PPWebKitDOMDOMImplementationClass = ^PWebKitDOMDOMImplementationClass;
  PWebKitDOMDOMImplementationClass = ^TWebKitDOMDOMImplementationClass;
  TWebKitDOMDOMImplementationClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMDOMPlugin = ^PWebKitDOMDOMPlugin;
  PWebKitDOMDOMPlugin = ^TWebKitDOMDOMPlugin;

  PPWebKitDOMDOMMimeType = ^PWebKitDOMDOMMimeType;
  PWebKitDOMDOMMimeType = ^TWebKitDOMDOMMimeType;
  TWebKitDOMDOMPlugin = object(TWebKitDOMObject)
    function get_description: Pgchar; cdecl; inline;
    function get_filename: Pgchar; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function item(index: gulong): PWebKitDOMDOMMimeType; cdecl; inline;
    function named_item(name: Pgchar): PWebKitDOMDOMMimeType; cdecl; inline;
    property description:  Pgchar read get_description ;
    property filename:  Pgchar read get_filename ;
    property length:  gulong read get_length ;
    property name:  Pgchar read get_name ;
  end;
  TWebKitDOMDOMMimeType = object(TWebKitDOMObject)
    function get_description: Pgchar; cdecl; inline;
    function get_enabled_plugin: PWebKitDOMDOMPlugin; cdecl; inline;
    function get_suffixes: Pgchar; cdecl; inline;
    property description:  Pgchar read get_description ;
    property enabled_plugin:  PWebKitDOMDOMPlugin read get_enabled_plugin ;
    property suffixes:  Pgchar read get_suffixes ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
  end;

  PPWebKitDOMDOMMimeTypeArray = ^PWebKitDOMDOMMimeTypeArray;
  PWebKitDOMDOMMimeTypeArray = ^TWebKitDOMDOMMimeTypeArray;
  TWebKitDOMDOMMimeTypeArray = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): PWebKitDOMDOMMimeType; cdecl; inline;
    function named_item(name: Pgchar): PWebKitDOMDOMMimeType; cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMDOMMimeTypeArrayClass = ^PWebKitDOMDOMMimeTypeArrayClass;
  PWebKitDOMDOMMimeTypeArrayClass = ^TWebKitDOMDOMMimeTypeArrayClass;
  TWebKitDOMDOMMimeTypeArrayClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMDOMMimeTypeClass = ^PWebKitDOMDOMMimeTypeClass;
  PWebKitDOMDOMMimeTypeClass = ^TWebKitDOMDOMMimeTypeClass;
  TWebKitDOMDOMMimeTypeClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMDOMPluginArray = ^PWebKitDOMDOMPluginArray;
  PWebKitDOMDOMPluginArray = ^TWebKitDOMDOMPluginArray;
  TWebKitDOMDOMPluginArray = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): PWebKitDOMDOMPlugin; cdecl; inline;
    function named_item(name: Pgchar): PWebKitDOMDOMPlugin; cdecl; inline;
    procedure refresh(reload: gboolean); cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMDOMPluginArrayClass = ^PWebKitDOMDOMPluginArrayClass;
  PWebKitDOMDOMPluginArrayClass = ^TWebKitDOMDOMPluginArrayClass;
  TWebKitDOMDOMPluginArrayClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMDOMPluginClass = ^PWebKitDOMDOMPluginClass;
  PWebKitDOMDOMPluginClass = ^TWebKitDOMDOMPluginClass;
  TWebKitDOMDOMPluginClass = object
    parent_class: TWebKitDOMObjectClass;
  end;
  TWebKitDOMRange = object(TWebKitDOMObject)
    function clone_contents: PWebKitDOMDocumentFragment; cdecl; inline;
    function clone_range: PWebKitDOMRange; cdecl; inline;
    procedure collapse(to_start: gboolean); cdecl; inline;
    function compare_boundary_points(how: gushort; source_range: PWebKitDOMRange): Tgshort; cdecl; inline;
    function compare_node(ref_node: PWebKitDOMNode): Tgshort; cdecl; inline;
    function compare_point(ref_node: PWebKitDOMNode; offset: glong): Tgshort; cdecl; inline;
    function create_contextual_fragment(html: Pgchar): PWebKitDOMDocumentFragment; cdecl; inline;
    procedure delete_contents; cdecl; inline;
    procedure detach; cdecl; inline;
    procedure expand(unit_: Pgchar); cdecl; inline;
    function extract_contents: PWebKitDOMDocumentFragment; cdecl; inline;
    function get_collapsed: gboolean; cdecl; inline;
    function get_common_ancestor_container: PWebKitDOMNode; cdecl; inline;
    function get_end_container: PWebKitDOMNode; cdecl; inline;
    function get_end_offset: glong; cdecl; inline;
    function get_start_container: PWebKitDOMNode; cdecl; inline;
    function get_start_offset: glong; cdecl; inline;
    function get_text: Pgchar; cdecl; inline;
    procedure insert_node(new_node: PWebKitDOMNode); cdecl; inline;
    function intersects_node(ref_node: PWebKitDOMNode): gboolean; cdecl; inline;
    function is_point_in_range(ref_node: PWebKitDOMNode; offset: glong): gboolean; cdecl; inline;
    procedure select_node(ref_node: PWebKitDOMNode); cdecl; inline;
    procedure select_node_contents(ref_node: PWebKitDOMNode); cdecl; inline;
    procedure set_end(ref_node: PWebKitDOMNode; offset: glong); cdecl; inline;
    procedure set_end_after(ref_node: PWebKitDOMNode); cdecl; inline;
    procedure set_end_before(ref_node: PWebKitDOMNode); cdecl; inline;
    procedure set_start(ref_node: PWebKitDOMNode; offset: glong); cdecl; inline;
    procedure set_start_after(ref_node: PWebKitDOMNode); cdecl; inline;
    procedure set_start_before(ref_node: PWebKitDOMNode); cdecl; inline;
    procedure surround_contents(new_parent: PWebKitDOMNode); cdecl; inline;
    function to_string: Pgchar; cdecl; inline;
    property collapsed:  gboolean read get_collapsed ;
    property common_ancestor_container:  PWebKitDOMNode read get_common_ancestor_container ;
    property end_container:  PWebKitDOMNode read get_end_container ;
    property end_offset:  glong read get_end_offset ;
    property start_container:  PWebKitDOMNode read get_start_container ;
    property start_offset:  glong read get_start_offset ;
    property text:  Pgchar read get_text ;
  end;

  PPWebKitDOMDOMSelection = ^PWebKitDOMDOMSelection;
  PWebKitDOMDOMSelection = ^TWebKitDOMDOMSelection;
  TWebKitDOMDOMSelection = object(TWebKitDOMObject)
    procedure add_range(range: PWebKitDOMRange); cdecl; inline;
    procedure collapse(node: PWebKitDOMNode; index: glong); cdecl; inline;
    procedure collapse_to_end; cdecl; inline;
    procedure collapse_to_start; cdecl; inline;
    function contains_node(node: PWebKitDOMNode; allow_partial: gboolean): gboolean; cdecl; inline;
    procedure delete_from_document; cdecl; inline;
    procedure empty; cdecl; inline;
    procedure extend(node: PWebKitDOMNode; offset: glong); cdecl; inline;
    function get_anchor_node: PWebKitDOMNode; cdecl; inline;
    function get_anchor_offset: glong; cdecl; inline;
    function get_base_node: PWebKitDOMNode; cdecl; inline;
    function get_base_offset: glong; cdecl; inline;
    function get_extent_node: PWebKitDOMNode; cdecl; inline;
    function get_extent_offset: glong; cdecl; inline;
    function get_focus_node: PWebKitDOMNode; cdecl; inline;
    function get_focus_offset: glong; cdecl; inline;
    function get_is_collapsed: gboolean; cdecl; inline;
    function get_range_at(index: glong): PWebKitDOMRange; cdecl; inline;
    function get_range_count: glong; cdecl; inline;
    procedure modify(alter: Pgchar; direction: Pgchar; granularity: Pgchar); cdecl; inline;
    procedure remove_all_ranges; cdecl; inline;
    procedure select_all_children(node: PWebKitDOMNode); cdecl; inline;
    procedure set_base_and_extent(base_node: PWebKitDOMNode; base_offset: glong; extent_node: PWebKitDOMNode; extent_offset: glong); cdecl; inline;
    procedure set_position(node: PWebKitDOMNode; offset: glong); cdecl; inline;
    property anchor_node:  PWebKitDOMNode read get_anchor_node ;
    property anchor_offset:  glong read get_anchor_offset ;
    property base_node:  PWebKitDOMNode read get_base_node ;
    property base_offset:  glong read get_base_offset ;
    property extent_node:  PWebKitDOMNode read get_extent_node ;
    property extent_offset:  glong read get_extent_offset ;
    property focus_node:  PWebKitDOMNode read get_focus_node ;
    property focus_offset:  glong read get_focus_offset ;
    property is_collapsed:  gboolean read get_is_collapsed ;
    property range_count:  glong read get_range_count ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
  end;

  PPWebKitDOMDOMSelectionClass = ^PWebKitDOMDOMSelectionClass;
  PWebKitDOMDOMSelectionClass = ^TWebKitDOMDOMSelectionClass;
  TWebKitDOMDOMSelectionClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMDOMTokenList = ^PWebKitDOMDOMTokenList;
  PWebKitDOMDOMTokenList = ^TWebKitDOMDOMTokenList;
  TWebKitDOMDOMTokenList = object(TWebKitDOMObject)
    procedure add(token: Pgchar); cdecl; inline;
    function contains(token: Pgchar): gboolean; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): Pgchar; cdecl; inline;
    procedure remove(token: Pgchar); cdecl; inline;
    function toggle(token: Pgchar): gboolean; cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMDOMSettableTokenList = ^PWebKitDOMDOMSettableTokenList;
  PWebKitDOMDOMSettableTokenList = ^TWebKitDOMDOMSettableTokenList;
  TWebKitDOMDOMSettableTokenList = object(TWebKitDOMDOMTokenList)
    function get_value: Pgchar; cdecl; inline;
    procedure set_value(value: Pgchar); cdecl; inline;
    property value:  Pgchar read get_value  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMDOMTokenListClass = ^PWebKitDOMDOMTokenListClass;
  PWebKitDOMDOMTokenListClass = ^TWebKitDOMDOMTokenListClass;
  TWebKitDOMDOMTokenListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMDOMSettableTokenListClass = ^PWebKitDOMDOMSettableTokenListClass;
  PWebKitDOMDOMSettableTokenListClass = ^TWebKitDOMDOMSettableTokenListClass;
  TWebKitDOMDOMSettableTokenListClass = object
    parent_class: TWebKitDOMDOMTokenListClass;
  end;

  PPWebKitDOMDOMStringList = ^PWebKitDOMDOMStringList;
  PWebKitDOMDOMStringList = ^TWebKitDOMDOMStringList;
  TWebKitDOMDOMStringList = object(TWebKitDOMObject)
    function contains(string_: Pgchar): gboolean; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): Pgchar; cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMDOMStringListClass = ^PWebKitDOMDOMStringListClass;
  PWebKitDOMDOMStringListClass = ^TWebKitDOMDOMStringListClass;
  TWebKitDOMDOMStringListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMDOMStringMap = ^PWebKitDOMDOMStringMap;
  PWebKitDOMDOMStringMap = ^TWebKitDOMDOMStringMap;
  TWebKitDOMDOMStringMap = object(TWebKitDOMObject)
  end;

  PPWebKitDOMDOMStringMapClass = ^PWebKitDOMDOMStringMapClass;
  PWebKitDOMDOMStringMapClass = ^TWebKitDOMDOMStringMapClass;
  TWebKitDOMDOMStringMapClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMHistory = ^PWebKitDOMHistory;
  PWebKitDOMHistory = ^TWebKitDOMHistory;
  TWebKitDOMHistory = object(TWebKitDOMObject)
    procedure back; cdecl; inline;
    procedure forward; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    procedure go(distance: glong); cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMStorage = ^PWebKitDOMStorage;
  PWebKitDOMStorage = ^TWebKitDOMStorage;
  TWebKitDOMStorage = object(TWebKitDOMObject)
    procedure clear; cdecl; inline;
    function get_item(key: Pgchar): Pgchar; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    function key(index: gulong): Pgchar; cdecl; inline;
    procedure remove_item(key: Pgchar); cdecl; inline;
    procedure set_item(key: Pgchar; data: Pgchar); cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMStyleMedia = ^PWebKitDOMStyleMedia;
  PWebKitDOMStyleMedia = ^TWebKitDOMStyleMedia;
  TWebKitDOMStyleMedia = object(TWebKitDOMObject)
    function match_medium(mediaquery: Pgchar): gboolean; cdecl; inline;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
  end;

  PPWebKitDOMMediaQueryList = ^PWebKitDOMMediaQueryList;
  PWebKitDOMMediaQueryList = ^TWebKitDOMMediaQueryList;

  PPWebKitDOMWebKitPoint = ^PWebKitDOMWebKitPoint;
  PWebKitDOMWebKitPoint = ^TWebKitDOMWebKitPoint;
  TWebKitDOMDOMWindow = object(TWebKitDOMObject)
    procedure alert(message: Pgchar); cdecl; inline;
    function atob(string_: Pgchar): Pgchar; cdecl; inline;
    procedure blur; cdecl; inline;
    function btoa(string_: Pgchar): Pgchar; cdecl; inline;
    procedure capture_events; cdecl; inline;
    procedure clear_interval(handle: glong); cdecl; inline;
    procedure clear_timeout(handle: glong); cdecl; inline;
    procedure close; cdecl; inline;
    function confirm(message: Pgchar): gboolean; cdecl; inline;
    function dispatch_event(evt: PWebKitDOMEvent): gboolean; cdecl; inline;
    function find(string_: Pgchar; case_sensitive: gboolean; backwards: gboolean; wrap: gboolean; whole_word: gboolean; search_in_frames: gboolean; show_dialog: gboolean): gboolean; cdecl; inline;
    procedure focus; cdecl; inline;
    function get_application_cache: PWebKitDOMDOMApplicationCache; cdecl; inline;
    function get_closed: gboolean; cdecl; inline;
    function get_computed_style(element: PWebKitDOMElement; pseudo_element: Pgchar): PWebKitDOMCSSStyleDeclaration; cdecl; inline;
    function get_default_status: Pgchar; cdecl; inline;
    function get_document: PWebKitDOMDocument; cdecl; inline;
    function get_frame_element: PWebKitDOMElement; cdecl; inline;
    function get_history: PWebKitDOMHistory; cdecl; inline;
    function get_local_storage: PWebKitDOMStorage; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_page_x_offset: glong; cdecl; inline;
    function get_page_y_offset: glong; cdecl; inline;
    function get_selection: PWebKitDOMDOMSelection; cdecl; inline;
    function get_session_storage: PWebKitDOMStorage; cdecl; inline;
    function get_status: Pgchar; cdecl; inline;
    function get_style_media: PWebKitDOMStyleMedia; cdecl; inline;
    function get_window: PWebKitDOMDOMWindow; cdecl; inline;
    function match_media(query: Pgchar): PWebKitDOMMediaQueryList; cdecl; inline;
    procedure move_by(x: gfloat; y: gfloat); cdecl; inline;
    procedure move_to(x: gfloat; y: gfloat); cdecl; inline;
    procedure print; cdecl; inline;
    function prompt(message: Pgchar; default_value: Pgchar): Pgchar; cdecl; inline;
    procedure release_events; cdecl; inline;
    procedure resize_by(x: gfloat; y: gfloat); cdecl; inline;
    procedure resize_to(width: gfloat; height: gfloat); cdecl; inline;
    procedure scroll(x: glong; y: glong); cdecl; inline;
    procedure scroll_by(x: glong; y: glong); cdecl; inline;
    procedure scroll_to(x: glong; y: glong); cdecl; inline;
    procedure set_default_status(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_status(value: Pgchar); cdecl; inline;
    procedure stop; cdecl; inline;
    function webkit_convert_point_from_node_to_page(node: PWebKitDOMNode; p: PWebKitDOMWebKitPoint): PWebKitDOMWebKitPoint; cdecl; inline;
    function webkit_convert_point_from_page_to_node(node: PWebKitDOMNode; p: PWebKitDOMWebKitPoint): PWebKitDOMWebKitPoint; cdecl; inline;
    property application_cache:  PWebKitDOMDOMApplicationCache read get_application_cache ;
    property closed:  gboolean read get_closed ;
    property default_status:  Pgchar read get_default_status  { property is writeable but setter not declared } ;
    property document:  PWebKitDOMDocument read get_document ;
    property frame_element:  PWebKitDOMElement read get_frame_element ;
    property history:  PWebKitDOMHistory read get_history ;
    property local_storage:  PWebKitDOMStorage read get_local_storage ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property page_x_offset:  glong read get_page_x_offset ;
    property page_y_offset:  glong read get_page_y_offset ;
    property session_storage:  PWebKitDOMStorage read get_session_storage ;
    property status:  Pgchar read get_status  { property is writeable but setter not declared } ;
    property style_media:  PWebKitDOMStyleMedia read get_style_media ;
    property window:  PWebKitDOMDOMWindow read get_window ;
  end;
  TWebKitDOMMediaQueryList = object(TWebKitDOMObject)
    function get_matches: gboolean; cdecl; inline;
    function get_media: Pgchar; cdecl; inline;
    property matches:  gboolean read get_matches ;
    property media:  Pgchar read get_media ;
  end;
  TWebKitDOMWebKitPoint = object(TWebKitDOMObject)
    function get_x: gfloat; cdecl; inline;
    function get_y: gfloat; cdecl; inline;
    procedure set_x(value: gfloat); cdecl; inline;
    procedure set_y(value: gfloat); cdecl; inline;
    property x:  gfloat read get_x  { property is writeable but setter not declared } ;
    property y:  gfloat read get_y  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMDOMWindowClass = ^PWebKitDOMDOMWindowClass;
  PWebKitDOMDOMWindowClass = ^TWebKitDOMDOMWindowClass;
  TWebKitDOMDOMWindowClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMDatabase = ^PWebKitDOMDatabase;
  PWebKitDOMDatabase = ^TWebKitDOMDatabase;
  TWebKitDOMDatabase = object(TWebKitDOMObject)
    function get_version: Pgchar; cdecl; inline;
    property version:  Pgchar read get_version ;
  end;

  PPWebKitDOMDatabaseClass = ^PWebKitDOMDatabaseClass;
  PWebKitDOMDatabaseClass = ^TWebKitDOMDatabaseClass;
  TWebKitDOMDatabaseClass = object
    parent_class: TWebKitDOMObjectClass;
  end;
  TWebKitDOMDocumentFragment = object(TWebKitDOMNode)
    function query_selector(selectors: Pgchar): PWebKitDOMElement; cdecl; inline;
    function query_selector_all(selectors: Pgchar): PWebKitDOMNodeList; cdecl; inline;
  end;
  TWebKitDOMEntityReference = object(TWebKitDOMNode)
  end;
  TWebKitDOMXPathExpression = object(TWebKitDOMObject)
    function evaluate(context_node: PWebKitDOMNode; type_: gushort; in_result: PWebKitDOMXPathResult): PWebKitDOMXPathResult; cdecl; inline;
  end;
  TWebKitDOMXPathNSResolver = object(TWebKitDOMObject)
    function lookup_namespace_uri(prefix: Pgchar): Pgchar; cdecl; inline;
  end;
  TWebKitDOMNodeIterator = object(TWebKitDOMObject)
    procedure detach; cdecl; inline;
    function get_expand_entity_references: gboolean; cdecl; inline;
    function get_filter: PWebKitDOMNodeFilter; cdecl; inline;
    function get_pointer_before_reference_node: gboolean; cdecl; inline;
    function get_reference_node: PWebKitDOMNode; cdecl; inline;
    function get_root: PWebKitDOMNode; cdecl; inline;
    function get_what_to_show: gulong; cdecl; inline;
    function next_node: PWebKitDOMNode; cdecl; inline;
    function previous_node: PWebKitDOMNode; cdecl; inline;
    property expand_entity_references:  gboolean read get_expand_entity_references ;
    property filter:  PWebKitDOMNodeFilter read get_filter ;
    property pointer_before_reference_node:  gboolean read get_pointer_before_reference_node ;
    property reference_node:  PWebKitDOMNode read get_reference_node ;
    property root:  PWebKitDOMNode read get_root ;
    property what_to_show:  gulong read get_what_to_show ;
  end;
  TWebKitDOMNodeFilter = object(TWebKitDOMObject)
    function accept_node(n: PWebKitDOMNode): Tgshort; cdecl; inline;
  end;
  TWebKitDOMProcessingInstruction = object(TWebKitDOMNode)
    function get_data: Pgchar; cdecl; inline;
    function get_sheet: PWebKitDOMStyleSheet; cdecl; inline;
    function get_target: Pgchar; cdecl; inline;
    procedure set_data(value: Pgchar); cdecl; inline;
    property data:  Pgchar read get_data  { property is writeable but setter not declared } ;
    property sheet:  PWebKitDOMStyleSheet read get_sheet ;
    property target:  Pgchar read get_target ;
  end;
  TWebKitDOMTreeWalker = object(TWebKitDOMObject)
    function first_child: PWebKitDOMNode; cdecl; inline;
    function get_current_node: PWebKitDOMNode; cdecl; inline;
    function get_expand_entity_references: gboolean; cdecl; inline;
    function get_filter: PWebKitDOMNodeFilter; cdecl; inline;
    function get_root: PWebKitDOMNode; cdecl; inline;
    function get_what_to_show: gulong; cdecl; inline;
    function last_child: PWebKitDOMNode; cdecl; inline;
    function next_node: PWebKitDOMNode; cdecl; inline;
    function next_sibling: PWebKitDOMNode; cdecl; inline;
    function parent_node: PWebKitDOMNode; cdecl; inline;
    function previous_node: PWebKitDOMNode; cdecl; inline;
    function previous_sibling: PWebKitDOMNode; cdecl; inline;
    procedure set_current_node(value: PWebKitDOMNode); cdecl; inline;
    property current_node:  PWebKitDOMNode read get_current_node  { property is writeable but setter not declared } ;
    property expand_entity_references:  gboolean read get_expand_entity_references ;
    property filter:  PWebKitDOMNodeFilter read get_filter ;
    property root:  PWebKitDOMNode read get_root ;
    property what_to_show:  gulong read get_what_to_show ;
  end;
  TWebKitDOMXPathResult = object(TWebKitDOMObject)
    function get_boolean_value: gboolean; cdecl; inline;
    function get_invalid_iterator_state: gboolean; cdecl; inline;
    function get_number_value: gdouble; cdecl; inline;
    function get_result_type: gushort; cdecl; inline;
    function get_single_node_value: PWebKitDOMNode; cdecl; inline;
    function get_snapshot_length: gulong; cdecl; inline;
    function get_string_value: Pgchar; cdecl; inline;
    function iterate_next: PWebKitDOMNode; cdecl; inline;
    function snapshot_item(index: gulong): PWebKitDOMNode; cdecl; inline;
    property boolean_value:  gboolean read get_boolean_value ;
    property invalid_iterator_state:  gboolean read get_invalid_iterator_state ;
    property number_value:  gdouble read get_number_value ;
    property result_type:  gushort read get_result_type ;
    property single_node_value:  PWebKitDOMNode read get_single_node_value ;
    property snapshot_length:  gulong read get_snapshot_length ;
    property string_value:  Pgchar read get_string_value ;
  end;
  TWebKitDOMHTMLCollection = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): PWebKitDOMNode; cdecl; inline;
    function named_item(name: Pgchar): PWebKitDOMNode; cdecl; inline;
    property length:  gulong read get_length ;
  end;
  TWebKitDOMHTMLElement = object(TWebKitDOMElement)
    function get_children: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_class_list: PWebKitDOMDOMTokenList; cdecl; inline;
    function get_class_name: Pgchar; cdecl; inline;
    function get_content_editable: Pgchar; cdecl; inline;
    function get_dir: Pgchar; cdecl; inline;
    function get_draggable: gboolean; cdecl; inline;
    function get_hidden: gboolean; cdecl; inline;
    function get_id: Pgchar; cdecl; inline;
    function get_inner_html: Pgchar; cdecl; inline;
    function get_inner_text: Pgchar; cdecl; inline;
    function get_is_content_editable: gboolean; cdecl; inline;
    function get_lang: Pgchar; cdecl; inline;
    function get_outer_html: Pgchar; cdecl; inline;
    function get_outer_text: Pgchar; cdecl; inline;
    function get_spellcheck: gboolean; cdecl; inline;
    function get_tab_index: glong; cdecl; inline;
    function get_title: Pgchar; cdecl; inline;
    function insert_adjacent_element(where: Pgchar; element: PWebKitDOMElement): PWebKitDOMElement; cdecl; inline;
    procedure insert_adjacent_html(where: Pgchar; html: Pgchar); cdecl; inline;
    procedure insert_adjacent_text(where: Pgchar; text: Pgchar); cdecl; inline;
    procedure set_class_name(value: Pgchar); cdecl; inline;
    procedure set_content_editable(value: Pgchar); cdecl; inline;
    procedure set_dir(value: Pgchar); cdecl; inline;
    procedure set_draggable(value: gboolean); cdecl; inline;
    procedure set_hidden(value: gboolean); cdecl; inline;
    procedure set_id(value: Pgchar); cdecl; inline;
    procedure set_inner_html(value: Pgchar); cdecl; inline;
    procedure set_inner_text(value: Pgchar); cdecl; inline;
    procedure set_lang(value: Pgchar); cdecl; inline;
    procedure set_outer_html(value: Pgchar); cdecl; inline;
    procedure set_outer_text(value: Pgchar); cdecl; inline;
    procedure set_spellcheck(value: gboolean); cdecl; inline;
    procedure set_tab_index(value: glong); cdecl; inline;
    procedure set_title(value: Pgchar); cdecl; inline;
    property children:  PWebKitDOMHTMLCollection read get_children ;
    property class_list:  PWebKitDOMDOMTokenList read get_class_list ;
    property class_name:  Pgchar read get_class_name  { property is writeable but setter not declared } ;
    property content_editable:  Pgchar read get_content_editable  { property is writeable but setter not declared } ;
    property dir:  Pgchar read get_dir  { property is writeable but setter not declared } ;
    property draggable:  gboolean read get_draggable  { property is writeable but setter not declared } ;
    property hidden:  gboolean read get_hidden  { property is writeable but setter not declared } ;
    property id:  Pgchar read get_id  { property is writeable but setter not declared } ;
    property inner_html:  Pgchar read get_inner_html  { property is writeable but setter not declared } ;
    property inner_text:  Pgchar read get_inner_text  { property is writeable but setter not declared } ;
    property is_content_editable:  gboolean read get_is_content_editable ;
    property lang:  Pgchar read get_lang  { property is writeable but setter not declared } ;
    property outer_html:  Pgchar read get_outer_html  { property is writeable but setter not declared } ;
    property outer_text:  Pgchar read get_outer_text  { property is writeable but setter not declared } ;
    property spellcheck:  gboolean read get_spellcheck  { property is writeable but setter not declared } ;
    property tab_index:  glong read get_tab_index  { property is writeable but setter not declared } ;
    property title:  Pgchar read get_title  { property is writeable but setter not declared } ;
  end;
  TWebKitDOMNodeList = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): PWebKitDOMNode; cdecl; inline;
    property length:  gulong read get_length ;
  end;
  TWebKitDOMHTMLHeadElement = object(TWebKitDOMHTMLElement)
    function get_profile: Pgchar; cdecl; inline;
    procedure set_profile(value: Pgchar); cdecl; inline;
    property profile:  Pgchar read get_profile  { property is writeable but setter not declared } ;
  end;
  TWebKitDOMStyleSheetList = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): PWebKitDOMStyleSheet; cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMDocumentClass = ^PWebKitDOMDocumentClass;
  PWebKitDOMDocumentClass = ^TWebKitDOMDocumentClass;
  TWebKitDOMDocumentClass = object
    parent_class: TWebKitDOMNodeClass;
  end;

  PPWebKitDOMDocumentFragmentClass = ^PWebKitDOMDocumentFragmentClass;
  PWebKitDOMDocumentFragmentClass = ^TWebKitDOMDocumentFragmentClass;
  TWebKitDOMDocumentFragmentClass = object
    parent_class: TWebKitDOMNodeClass;
  end;
  TWebKitDOMNamedNodeMap = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function get_named_item(name: Pgchar): PWebKitDOMNode; cdecl; inline;
    function get_named_item_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNode; cdecl; inline;
    function item(index: gulong): PWebKitDOMNode; cdecl; inline;
    function remove_named_item(name: Pgchar): PWebKitDOMNode; cdecl; inline;
    function remove_named_item_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNode; cdecl; inline;
    function set_named_item(node: PWebKitDOMNode): PWebKitDOMNode; cdecl; inline;
    function set_named_item_ns(node: PWebKitDOMNode): PWebKitDOMNode; cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMDocumentTypeClass = ^PWebKitDOMDocumentTypeClass;
  PWebKitDOMDocumentTypeClass = ^TWebKitDOMDocumentTypeClass;
  TWebKitDOMDocumentTypeClass = object
    parent_class: TWebKitDOMNodeClass;
  end;

  PPWebKitDOMWebKitAnimation = ^PWebKitDOMWebKitAnimation;
  PWebKitDOMWebKitAnimation = ^TWebKitDOMWebKitAnimation;
  TWebKitDOMWebKitAnimationList = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): PWebKitDOMWebKitAnimation; cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMElementClass = ^PWebKitDOMElementClass;
  PWebKitDOMElementClass = ^TWebKitDOMElementClass;
  TWebKitDOMElementClass = object
    parent_class: TWebKitDOMNodeClass;
  end;

  PPWebKitDOMEntityReferenceClass = ^PWebKitDOMEntityReferenceClass;
  PWebKitDOMEntityReferenceClass = ^TWebKitDOMEntityReferenceClass;
  TWebKitDOMEntityReferenceClass = object
    parent_class: TWebKitDOMNodeClass;
  end;

  PPWebKitDOMEventClass = ^PWebKitDOMEventClass;
  PWebKitDOMEventClass = ^TWebKitDOMEventClass;
  TWebKitDOMEventClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMEventTargetClass = ^PWebKitDOMEventTargetClass;
  PWebKitDOMEventTargetClass = ^TWebKitDOMEventTargetClass;

  TWebKitDOMEventTargetClass = record
    Unknown: Pointer;
  end;



  PPWebKitDOMEventTargetIface = ^PWebKitDOMEventTargetIface;
  PWebKitDOMEventTargetIface = ^TWebKitDOMEventTargetIface;
  TWebKitDOMEventTargetIface = object
    gIface: TGTypeInterface;
    dispatch_event: procedure(target: PWebKitDOMEventTarget; event: PWebKitDOMEvent); cdecl;
    add_event_listener: function(target: PWebKitDOMEventTarget; eventName: Pgchar; handler: TGCallback; bubble: gboolean; userData: gpointer): gboolean; cdecl;
    remove_event_listener: function(target: PWebKitDOMEventTarget; eventName: Pgchar; handler: TGCallback; bubble: gboolean): gboolean; cdecl;
  end;

  PPWebKitDOMFile = ^PWebKitDOMFile;
  PWebKitDOMFile = ^TWebKitDOMFile;
  TWebKitDOMFile = object(TWebKitDOMBlob)
    function get_file_name: Pgchar; cdecl; inline;
    function get_file_size: guint64; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    property file_name:  Pgchar read get_file_name ;
    property file_size:  guint64 read get_file_size ;
    property name:  Pgchar read get_name ;
  end;

  PPWebKitDOMFileClass = ^PWebKitDOMFileClass;
  PWebKitDOMFileClass = ^TWebKitDOMFileClass;
  TWebKitDOMFileClass = object
    parent_class: TWebKitDOMBlobClass;
  end;

  PPWebKitDOMFileList = ^PWebKitDOMFileList;
  PWebKitDOMFileList = ^TWebKitDOMFileList;
  TWebKitDOMFileList = object(TWebKitDOMObject)
    function get_length: gulong; cdecl; inline;
    function item(index: gulong): PWebKitDOMFile; cdecl; inline;
    property length:  gulong read get_length ;
  end;

  PPWebKitDOMFileListClass = ^PWebKitDOMFileListClass;
  PWebKitDOMFileListClass = ^TWebKitDOMFileListClass;
  TWebKitDOMFileListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMHTMLAnchorElement = ^PWebKitDOMHTMLAnchorElement;
  PWebKitDOMHTMLAnchorElement = ^TWebKitDOMHTMLAnchorElement;
  TWebKitDOMHTMLAnchorElement = object(TWebKitDOMHTMLElement)
    function get_access_key: Pgchar; cdecl; inline;
    function get_charset: Pgchar; cdecl; inline;
    function get_coords: Pgchar; cdecl; inline;
    function get_hash: Pgchar; cdecl; inline;
    function get_host: Pgchar; cdecl; inline;
    function get_hostname: Pgchar; cdecl; inline;
    function get_href: Pgchar; cdecl; inline;
    function get_hreflang: Pgchar; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_origin: Pgchar; cdecl; inline;
    function get_parameter(name: Pgchar): Pgchar; cdecl; inline;
    function get_pathname: Pgchar; cdecl; inline;
    function get_port: Pgchar; cdecl; inline;
    function get_protocol: Pgchar; cdecl; inline;
    function get_rel: Pgchar; cdecl; inline;
    function get_rev: Pgchar; cdecl; inline;
    function get_search: Pgchar; cdecl; inline;
    function get_shape: Pgchar; cdecl; inline;
    function get_target: Pgchar; cdecl; inline;
    function get_text: Pgchar; cdecl; inline;
    procedure set_access_key(value: Pgchar); cdecl; inline;
    procedure set_charset(value: Pgchar); cdecl; inline;
    procedure set_coords(value: Pgchar); cdecl; inline;
    procedure set_hash(value: Pgchar); cdecl; inline;
    procedure set_host(value: Pgchar); cdecl; inline;
    procedure set_hostname(value: Pgchar); cdecl; inline;
    procedure set_href(value: Pgchar); cdecl; inline;
    procedure set_hreflang(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_pathname(value: Pgchar); cdecl; inline;
    procedure set_port(value: Pgchar); cdecl; inline;
    procedure set_protocol(value: Pgchar); cdecl; inline;
    procedure set_rel(value: Pgchar); cdecl; inline;
    procedure set_rev(value: Pgchar); cdecl; inline;
    procedure set_search(value: Pgchar); cdecl; inline;
    procedure set_shape(value: Pgchar); cdecl; inline;
    procedure set_target(value: Pgchar); cdecl; inline;
    property access_key:  Pgchar read get_access_key  { property is writeable but setter not declared } ;
    property charset:  Pgchar read get_charset  { property is writeable but setter not declared } ;
    property coords:  Pgchar read get_coords  { property is writeable but setter not declared } ;
    property hash:  Pgchar read get_hash  { property is writeable but setter not declared } ;
    property host:  Pgchar read get_host  { property is writeable but setter not declared } ;
    property hostname:  Pgchar read get_hostname  { property is writeable but setter not declared } ;
    property href:  Pgchar read get_href  { property is writeable but setter not declared } ;
    property hreflang:  Pgchar read get_hreflang  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property origin:  Pgchar read get_origin ;
    property pathname:  Pgchar read get_pathname  { property is writeable but setter not declared } ;
    property port:  Pgchar read get_port  { property is writeable but setter not declared } ;
    property protocol:  Pgchar read get_protocol  { property is writeable but setter not declared } ;
    property rel:  Pgchar read get_rel  { property is writeable but setter not declared } ;
    property rev:  Pgchar read get_rev  { property is writeable but setter not declared } ;
    property search:  Pgchar read get_search  { property is writeable but setter not declared } ;
    property shape:  Pgchar read get_shape  { property is writeable but setter not declared } ;
    property target:  Pgchar read get_target  { property is writeable but setter not declared } ;
    property text:  Pgchar read get_text ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLElementClass = ^PWebKitDOMHTMLElementClass;
  PWebKitDOMHTMLElementClass = ^TWebKitDOMHTMLElementClass;
  TWebKitDOMHTMLElementClass = object
    parent_class: TWebKitDOMElementClass;
  end;

  PPWebKitDOMHTMLAnchorElementClass = ^PWebKitDOMHTMLAnchorElementClass;
  PWebKitDOMHTMLAnchorElementClass = ^TWebKitDOMHTMLAnchorElementClass;
  TWebKitDOMHTMLAnchorElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLAppletElement = ^PWebKitDOMHTMLAppletElement;
  PWebKitDOMHTMLAppletElement = ^TWebKitDOMHTMLAppletElement;
  TWebKitDOMHTMLAppletElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    function get_alt: Pgchar; cdecl; inline;
    function get_archive: Pgchar; cdecl; inline;
    function get_code: Pgchar; cdecl; inline;
    function get_code_base: Pgchar; cdecl; inline;
    function get_height: Pgchar; cdecl; inline;
    function get_hspace: glong; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_object: Pgchar; cdecl; inline;
    function get_vspace: glong; cdecl; inline;
    function get_width: Pgchar; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_alt(value: Pgchar); cdecl; inline;
    procedure set_archive(value: Pgchar); cdecl; inline;
    procedure set_code(value: Pgchar); cdecl; inline;
    procedure set_code_base(value: Pgchar); cdecl; inline;
    procedure set_height(value: Pgchar); cdecl; inline;
    procedure set_hspace(value: glong); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_object(value: Pgchar); cdecl; inline;
    procedure set_vspace(value: glong); cdecl; inline;
    procedure set_width(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property alt:  Pgchar read get_alt  { property is writeable but setter not declared } ;
    property archive:  Pgchar read get_archive  { property is writeable but setter not declared } ;
    property code:  Pgchar read get_code  { property is writeable but setter not declared } ;
    property code_base:  Pgchar read get_code_base  { property is writeable but setter not declared } ;
    property height:  Pgchar read get_height  { property is writeable but setter not declared } ;
    property hspace:  glong read get_hspace  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property object_:  Pgchar read get_object  { property is writeable but setter not declared } ;
    property vspace:  glong read get_vspace  { property is writeable but setter not declared } ;
    property width:  Pgchar read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLAppletElementClass = ^PWebKitDOMHTMLAppletElementClass;
  PWebKitDOMHTMLAppletElementClass = ^TWebKitDOMHTMLAppletElementClass;
  TWebKitDOMHTMLAppletElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLAreaElement = ^PWebKitDOMHTMLAreaElement;
  PWebKitDOMHTMLAreaElement = ^TWebKitDOMHTMLAreaElement;
  TWebKitDOMHTMLAreaElement = object(TWebKitDOMHTMLElement)
    function get_access_key: Pgchar; cdecl; inline;
    function get_alt: Pgchar; cdecl; inline;
    function get_coords: Pgchar; cdecl; inline;
    function get_hash: Pgchar; cdecl; inline;
    function get_host: Pgchar; cdecl; inline;
    function get_hostname: Pgchar; cdecl; inline;
    function get_href: Pgchar; cdecl; inline;
    function get_no_href: gboolean; cdecl; inline;
    function get_pathname: Pgchar; cdecl; inline;
    function get_port: Pgchar; cdecl; inline;
    function get_protocol: Pgchar; cdecl; inline;
    function get_search: Pgchar; cdecl; inline;
    function get_shape: Pgchar; cdecl; inline;
    function get_target: Pgchar; cdecl; inline;
    procedure set_access_key(value: Pgchar); cdecl; inline;
    procedure set_alt(value: Pgchar); cdecl; inline;
    procedure set_coords(value: Pgchar); cdecl; inline;
    procedure set_href(value: Pgchar); cdecl; inline;
    procedure set_no_href(value: gboolean); cdecl; inline;
    procedure set_shape(value: Pgchar); cdecl; inline;
    procedure set_target(value: Pgchar); cdecl; inline;
    property access_key:  Pgchar read get_access_key  { property is writeable but setter not declared } ;
    property alt:  Pgchar read get_alt  { property is writeable but setter not declared } ;
    property coords:  Pgchar read get_coords  { property is writeable but setter not declared } ;
    property hash:  Pgchar read get_hash ;
    property host:  Pgchar read get_host ;
    property hostname:  Pgchar read get_hostname ;
    property href:  Pgchar read get_href  { property is writeable but setter not declared } ;
    property no_href:  gboolean read get_no_href  { property is writeable but setter not declared } ;
    property pathname:  Pgchar read get_pathname ;
    property port:  Pgchar read get_port ;
    property protocol:  Pgchar read get_protocol ;
    property search:  Pgchar read get_search ;
    property shape:  Pgchar read get_shape  { property is writeable but setter not declared } ;
    property target:  Pgchar read get_target  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLAreaElementClass = ^PWebKitDOMHTMLAreaElementClass;
  PWebKitDOMHTMLAreaElementClass = ^TWebKitDOMHTMLAreaElementClass;
  TWebKitDOMHTMLAreaElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLMediaElement = ^PWebKitDOMHTMLMediaElement;
  PWebKitDOMHTMLMediaElement = ^TWebKitDOMHTMLMediaElement;

  PPWebKitDOMTimeRanges = ^PWebKitDOMTimeRanges;
  PWebKitDOMTimeRanges = ^TWebKitDOMTimeRanges;

  PPWebKitDOMMediaError = ^PWebKitDOMMediaError;
  PWebKitDOMMediaError = ^TWebKitDOMMediaError;
  TWebKitDOMHTMLMediaElement = object(TWebKitDOMHTMLElement)
    function can_play_type(type_: Pgchar): Pgchar; cdecl; inline;
    function get_autoplay: gboolean; cdecl; inline;
    function get_buffered: PWebKitDOMTimeRanges; cdecl; inline;
    function get_controls: gboolean; cdecl; inline;
    function get_current_src: Pgchar; cdecl; inline;
    function get_current_time: gfloat; cdecl; inline;
    function get_default_playback_rate: gfloat; cdecl; inline;
    function get_duration: gfloat; cdecl; inline;
    function get_ended: gboolean; cdecl; inline;
    function get_error: PWebKitDOMMediaError; cdecl; inline;
    function get_loop: gboolean; cdecl; inline;
    function get_muted: gboolean; cdecl; inline;
    function get_network_state: gushort; cdecl; inline;
    function get_paused: gboolean; cdecl; inline;
    function get_playback_rate: gfloat; cdecl; inline;
    function get_played: PWebKitDOMTimeRanges; cdecl; inline;
    function get_preload: Pgchar; cdecl; inline;
    function get_ready_state: gushort; cdecl; inline;
    function get_seekable: PWebKitDOMTimeRanges; cdecl; inline;
    function get_seeking: gboolean; cdecl; inline;
    function get_src: Pgchar; cdecl; inline;
    function get_start_time: gfloat; cdecl; inline;
    function get_volume: gfloat; cdecl; inline;
    function get_webkit_closed_captions_visible: gboolean; cdecl; inline;
    function get_webkit_has_closed_captions: gboolean; cdecl; inline;
    function get_webkit_preserves_pitch: gboolean; cdecl; inline;
    procedure load(isUserGesture: gboolean); cdecl; inline;
    procedure pause(isUserGesture: gboolean); cdecl; inline;
    procedure play(isUserGesture: gboolean); cdecl; inline;
    procedure set_autoplay(value: gboolean); cdecl; inline;
    procedure set_controls(value: gboolean); cdecl; inline;
    procedure set_current_time(value: gfloat); cdecl; inline;
    procedure set_default_playback_rate(value: gfloat); cdecl; inline;
    procedure set_loop(value: gboolean); cdecl; inline;
    procedure set_muted(value: gboolean); cdecl; inline;
    procedure set_playback_rate(value: gfloat); cdecl; inline;
    procedure set_preload(value: Pgchar); cdecl; inline;
    procedure set_src(value: Pgchar); cdecl; inline;
    procedure set_volume(value: gfloat); cdecl; inline;
    procedure set_webkit_closed_captions_visible(value: gboolean); cdecl; inline;
    procedure set_webkit_preserves_pitch(value: gboolean); cdecl; inline;
    property autoplay:  gboolean read get_autoplay  { property is writeable but setter not declared } ;
    property buffered:  PWebKitDOMTimeRanges read get_buffered ;
    property controls:  gboolean read get_controls  { property is writeable but setter not declared } ;
    property current_src:  Pgchar read get_current_src ;
    property current_time:  gfloat read get_current_time  { property is writeable but setter not declared } ;
    property default_playback_rate:  gfloat read get_default_playback_rate  { property is writeable but setter not declared } ;
    property duration:  gfloat read get_duration ;
    property ended:  gboolean read get_ended ;
    property error:  PWebKitDOMMediaError read get_error ;
    property loop:  gboolean read get_loop  { property is writeable but setter not declared } ;
    property muted:  gboolean read get_muted  { property is writeable but setter not declared } ;
    property network_state:  gushort read get_network_state ;
    property paused:  gboolean read get_paused ;
    property playback_rate:  gfloat read get_playback_rate  { property is writeable but setter not declared } ;
    property played:  PWebKitDOMTimeRanges read get_played ;
    property preload:  Pgchar read get_preload  { property is writeable but setter not declared } ;
    property ready_state:  gushort read get_ready_state ;
    property seekable:  PWebKitDOMTimeRanges read get_seekable ;
    property seeking:  gboolean read get_seeking ;
    property src:  Pgchar read get_src  { property is writeable but setter not declared } ;
    property start_time:  gfloat read get_start_time ;
    property volume:  gfloat read get_volume  { property is writeable but setter not declared } ;
    property webkit_closed_captions_visible:  gboolean read get_webkit_closed_captions_visible  { property is writeable but setter not declared } ;
    property webkit_has_closed_captions:  gboolean read get_webkit_has_closed_captions ;
    property webkit_preserves_pitch:  gboolean read get_webkit_preserves_pitch  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLAudioElement = ^PWebKitDOMHTMLAudioElement;
  PWebKitDOMHTMLAudioElement = ^TWebKitDOMHTMLAudioElement;
  TWebKitDOMHTMLAudioElement = object(TWebKitDOMHTMLMediaElement)
  end;

  PPWebKitDOMHTMLMediaElementClass = ^PWebKitDOMHTMLMediaElementClass;
  PWebKitDOMHTMLMediaElementClass = ^TWebKitDOMHTMLMediaElementClass;
  TWebKitDOMHTMLMediaElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLAudioElementClass = ^PWebKitDOMHTMLAudioElementClass;
  PWebKitDOMHTMLAudioElementClass = ^TWebKitDOMHTMLAudioElementClass;
  TWebKitDOMHTMLAudioElementClass = object
    parent_class: TWebKitDOMHTMLMediaElementClass;
  end;

  PPWebKitDOMHTMLBRElement = ^PWebKitDOMHTMLBRElement;
  PWebKitDOMHTMLBRElement = ^TWebKitDOMHTMLBRElement;
  TWebKitDOMHTMLBRElement = object(TWebKitDOMHTMLElement)
    function get_clear: Pgchar; cdecl; inline;
    procedure set_clear(value: Pgchar); cdecl; inline;
    property clear:  Pgchar read get_clear  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLBRElementClass = ^PWebKitDOMHTMLBRElementClass;
  PWebKitDOMHTMLBRElementClass = ^TWebKitDOMHTMLBRElementClass;
  TWebKitDOMHTMLBRElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLBaseElement = ^PWebKitDOMHTMLBaseElement;
  PWebKitDOMHTMLBaseElement = ^TWebKitDOMHTMLBaseElement;
  TWebKitDOMHTMLBaseElement = object(TWebKitDOMHTMLElement)
    function get_href: Pgchar; cdecl; inline;
    function get_target: Pgchar; cdecl; inline;
    procedure set_href(value: Pgchar); cdecl; inline;
    procedure set_target(value: Pgchar); cdecl; inline;
    property href:  Pgchar read get_href  { property is writeable but setter not declared } ;
    property target:  Pgchar read get_target  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLBaseElementClass = ^PWebKitDOMHTMLBaseElementClass;
  PWebKitDOMHTMLBaseElementClass = ^TWebKitDOMHTMLBaseElementClass;
  TWebKitDOMHTMLBaseElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLBaseFontElement = ^PWebKitDOMHTMLBaseFontElement;
  PWebKitDOMHTMLBaseFontElement = ^TWebKitDOMHTMLBaseFontElement;
  TWebKitDOMHTMLBaseFontElement = object(TWebKitDOMHTMLElement)
    function get_color: Pgchar; cdecl; inline;
    function get_face: Pgchar; cdecl; inline;
    function get_size: glong; cdecl; inline;
    procedure set_color(value: Pgchar); cdecl; inline;
    procedure set_face(value: Pgchar); cdecl; inline;
    procedure set_size(value: glong); cdecl; inline;
    property color:  Pgchar read get_color  { property is writeable but setter not declared } ;
    property face:  Pgchar read get_face  { property is writeable but setter not declared } ;
    property size:  glong read get_size  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLBaseFontElementClass = ^PWebKitDOMHTMLBaseFontElementClass;
  PWebKitDOMHTMLBaseFontElementClass = ^TWebKitDOMHTMLBaseFontElementClass;
  TWebKitDOMHTMLBaseFontElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLBlockquoteElement = ^PWebKitDOMHTMLBlockquoteElement;
  PWebKitDOMHTMLBlockquoteElement = ^TWebKitDOMHTMLBlockquoteElement;
  TWebKitDOMHTMLBlockquoteElement = object(TWebKitDOMHTMLElement)
    function get_cite: Pgchar; cdecl; inline;
    procedure set_cite(value: Pgchar); cdecl; inline;
    property cite:  Pgchar read get_cite  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLBlockquoteElementClass = ^PWebKitDOMHTMLBlockquoteElementClass;
  PWebKitDOMHTMLBlockquoteElementClass = ^TWebKitDOMHTMLBlockquoteElementClass;
  TWebKitDOMHTMLBlockquoteElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLBodyElement = ^PWebKitDOMHTMLBodyElement;
  PWebKitDOMHTMLBodyElement = ^TWebKitDOMHTMLBodyElement;
  TWebKitDOMHTMLBodyElement = object(TWebKitDOMHTMLElement)
    function get_a_link: Pgchar; cdecl; inline;
    function get_background: Pgchar; cdecl; inline;
    function get_bg_color: Pgchar; cdecl; inline;
    function get_link: Pgchar; cdecl; inline;
    function get_text: Pgchar; cdecl; inline;
    function get_v_link: Pgchar; cdecl; inline;
    procedure set_a_link(value: Pgchar); cdecl; inline;
    procedure set_background(value: Pgchar); cdecl; inline;
    procedure set_bg_color(value: Pgchar); cdecl; inline;
    procedure set_link(value: Pgchar); cdecl; inline;
    procedure set_text(value: Pgchar); cdecl; inline;
    procedure set_v_link(value: Pgchar); cdecl; inline;
    property a_link:  Pgchar read get_a_link  { property is writeable but setter not declared } ;
    property background:  Pgchar read get_background  { property is writeable but setter not declared } ;
    property bg_color:  Pgchar read get_bg_color  { property is writeable but setter not declared } ;
    property link:  Pgchar read get_link  { property is writeable but setter not declared } ;
    property text:  Pgchar read get_text  { property is writeable but setter not declared } ;
    property v_link:  Pgchar read get_v_link  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLBodyElementClass = ^PWebKitDOMHTMLBodyElementClass;
  PWebKitDOMHTMLBodyElementClass = ^TWebKitDOMHTMLBodyElementClass;
  TWebKitDOMHTMLBodyElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLFormElement = ^PWebKitDOMHTMLFormElement;
  PWebKitDOMHTMLFormElement = ^TWebKitDOMHTMLFormElement;
  TWebKitDOMHTMLFormElement = object(TWebKitDOMHTMLElement)
    function check_validity: gboolean; cdecl; inline;
    procedure dispatch_form_change; cdecl; inline;
    procedure dispatch_form_input; cdecl; inline;
    function get_accept_charset: Pgchar; cdecl; inline;
    function get_action: Pgchar; cdecl; inline;
    function get_elements: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_encoding: Pgchar; cdecl; inline;
    function get_enctype: Pgchar; cdecl; inline;
    function get_length: glong; cdecl; inline;
    function get_method: Pgchar; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_no_validate: gboolean; cdecl; inline;
    function get_target: Pgchar; cdecl; inline;
    procedure reset; cdecl; inline;
    procedure set_accept_charset(value: Pgchar); cdecl; inline;
    procedure set_action(value: Pgchar); cdecl; inline;
    procedure set_encoding(value: Pgchar); cdecl; inline;
    procedure set_enctype(value: Pgchar); cdecl; inline;
    procedure set_method(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_no_validate(value: gboolean); cdecl; inline;
    procedure set_target(value: Pgchar); cdecl; inline;
    procedure submit; cdecl; inline;
    property accept_charset:  Pgchar read get_accept_charset  { property is writeable but setter not declared } ;
    property action:  Pgchar read get_action  { property is writeable but setter not declared } ;
    property elements:  PWebKitDOMHTMLCollection read get_elements ;
    property encoding:  Pgchar read get_encoding  { property is writeable but setter not declared } ;
    property enctype:  Pgchar read get_enctype  { property is writeable but setter not declared } ;
    property length:  glong read get_length ;
    property method:  Pgchar read get_method  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property no_validate:  gboolean read get_no_validate  { property is writeable but setter not declared } ;
    property target:  Pgchar read get_target  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMValidityState = ^PWebKitDOMValidityState;
  PWebKitDOMValidityState = ^TWebKitDOMValidityState;
  TWebKitDOMValidityState = object(TWebKitDOMObject)
    function get_custom_error: gboolean; cdecl; inline;
    function get_pattern_mismatch: gboolean; cdecl; inline;
    function get_range_overflow: gboolean; cdecl; inline;
    function get_range_underflow: gboolean; cdecl; inline;
    function get_step_mismatch: gboolean; cdecl; inline;
    function get_too_long: gboolean; cdecl; inline;
    function get_type_mismatch: gboolean; cdecl; inline;
    function get_valid: gboolean; cdecl; inline;
    function get_value_missing: gboolean; cdecl; inline;
    property custom_error:  gboolean read get_custom_error ;
    property pattern_mismatch:  gboolean read get_pattern_mismatch ;
    property range_overflow:  gboolean read get_range_overflow ;
    property range_underflow:  gboolean read get_range_underflow ;
    property step_mismatch:  gboolean read get_step_mismatch ;
    property too_long:  gboolean read get_too_long ;
    property type_mismatch:  gboolean read get_type_mismatch ;
    property valid:  gboolean read get_valid ;
    property value_missing:  gboolean read get_value_missing ;
  end;

  PPWebKitDOMHTMLButtonElement = ^PWebKitDOMHTMLButtonElement;
  PWebKitDOMHTMLButtonElement = ^TWebKitDOMHTMLButtonElement;
  TWebKitDOMHTMLButtonElement = object(TWebKitDOMHTMLElement)
    function check_validity: gboolean; cdecl; inline;
    procedure click; cdecl; inline;
    function get_access_key: Pgchar; cdecl; inline;
    function get_autofocus: gboolean; cdecl; inline;
    function get_disabled: gboolean; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_form_action: Pgchar; cdecl; inline;
    function get_form_enctype: Pgchar; cdecl; inline;
    function get_form_method: Pgchar; cdecl; inline;
    function get_form_no_validate: gboolean; cdecl; inline;
    function get_form_target: Pgchar; cdecl; inline;
    function get_labels: PWebKitDOMNodeList; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_validation_message: Pgchar; cdecl; inline;
    function get_validity: PWebKitDOMValidityState; cdecl; inline;
    function get_value: Pgchar; cdecl; inline;
    function get_will_validate: gboolean; cdecl; inline;
    procedure set_access_key(value: Pgchar); cdecl; inline;
    procedure set_autofocus(value: gboolean); cdecl; inline;
    procedure set_custom_validity(error: Pgchar); cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_form_action(value: Pgchar); cdecl; inline;
    procedure set_form_enctype(value: Pgchar); cdecl; inline;
    procedure set_form_method(value: Pgchar); cdecl; inline;
    procedure set_form_no_validate(value: gboolean); cdecl; inline;
    procedure set_form_target(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_value(value: Pgchar); cdecl; inline;
    property access_key:  Pgchar read get_access_key  { property is writeable but setter not declared } ;
    property autofocus:  gboolean read get_autofocus  { property is writeable but setter not declared } ;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property form_action:  Pgchar read get_form_action  { property is writeable but setter not declared } ;
    property form_enctype:  Pgchar read get_form_enctype  { property is writeable but setter not declared } ;
    property form_method:  Pgchar read get_form_method  { property is writeable but setter not declared } ;
    property form_no_validate:  gboolean read get_form_no_validate  { property is writeable but setter not declared } ;
    property form_target:  Pgchar read get_form_target  { property is writeable but setter not declared } ;
    property labels:  PWebKitDOMNodeList read get_labels ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
    property validation_message:  Pgchar read get_validation_message ;
    property validity:  PWebKitDOMValidityState read get_validity ;
    property value:  Pgchar read get_value  { property is writeable but setter not declared } ;
    property will_validate:  gboolean read get_will_validate ;
  end;

  PPWebKitDOMHTMLButtonElementClass = ^PWebKitDOMHTMLButtonElementClass;
  PWebKitDOMHTMLButtonElementClass = ^TWebKitDOMHTMLButtonElementClass;
  TWebKitDOMHTMLButtonElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLCanvasElement = ^PWebKitDOMHTMLCanvasElement;
  PWebKitDOMHTMLCanvasElement = ^TWebKitDOMHTMLCanvasElement;
  TWebKitDOMHTMLCanvasElement = object(TWebKitDOMHTMLElement)
    function get_height: glong; cdecl; inline;
    function get_width: glong; cdecl; inline;
    procedure set_height(value: glong); cdecl; inline;
    procedure set_width(value: glong); cdecl; inline;
    property height:  glong read get_height  { property is writeable but setter not declared } ;
    property width:  glong read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLCanvasElementClass = ^PWebKitDOMHTMLCanvasElementClass;
  PWebKitDOMHTMLCanvasElementClass = ^TWebKitDOMHTMLCanvasElementClass;
  TWebKitDOMHTMLCanvasElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLCollectionClass = ^PWebKitDOMHTMLCollectionClass;
  PWebKitDOMHTMLCollectionClass = ^TWebKitDOMHTMLCollectionClass;
  TWebKitDOMHTMLCollectionClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMHTMLDListElement = ^PWebKitDOMHTMLDListElement;
  PWebKitDOMHTMLDListElement = ^TWebKitDOMHTMLDListElement;
  TWebKitDOMHTMLDListElement = object(TWebKitDOMHTMLElement)
    function get_compact: gboolean; cdecl; inline;
    procedure set_compact(value: gboolean); cdecl; inline;
    property compact:  gboolean read get_compact  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLDListElementClass = ^PWebKitDOMHTMLDListElementClass;
  PWebKitDOMHTMLDListElementClass = ^TWebKitDOMHTMLDListElementClass;
  TWebKitDOMHTMLDListElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLDetailsElement = ^PWebKitDOMHTMLDetailsElement;
  PWebKitDOMHTMLDetailsElement = ^TWebKitDOMHTMLDetailsElement;
  TWebKitDOMHTMLDetailsElement = object(TWebKitDOMHTMLElement)
    function get_open: gboolean; cdecl; inline;
    procedure set_open(value: gboolean); cdecl; inline;
    property open:  gboolean read get_open  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLDetailsElementClass = ^PWebKitDOMHTMLDetailsElementClass;
  PWebKitDOMHTMLDetailsElementClass = ^TWebKitDOMHTMLDetailsElementClass;
  TWebKitDOMHTMLDetailsElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLDirectoryElement = ^PWebKitDOMHTMLDirectoryElement;
  PWebKitDOMHTMLDirectoryElement = ^TWebKitDOMHTMLDirectoryElement;
  TWebKitDOMHTMLDirectoryElement = object(TWebKitDOMHTMLElement)
    function get_compact: gboolean; cdecl; inline;
    procedure set_compact(value: gboolean); cdecl; inline;
    property compact:  gboolean read get_compact  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLDirectoryElementClass = ^PWebKitDOMHTMLDirectoryElementClass;
  PWebKitDOMHTMLDirectoryElementClass = ^TWebKitDOMHTMLDirectoryElementClass;
  TWebKitDOMHTMLDirectoryElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLDivElement = ^PWebKitDOMHTMLDivElement;
  PWebKitDOMHTMLDivElement = ^TWebKitDOMHTMLDivElement;
  TWebKitDOMHTMLDivElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLDivElementClass = ^PWebKitDOMHTMLDivElementClass;
  PWebKitDOMHTMLDivElementClass = ^TWebKitDOMHTMLDivElementClass;
  TWebKitDOMHTMLDivElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLDocumentClass = ^PWebKitDOMHTMLDocumentClass;
  PWebKitDOMHTMLDocumentClass = ^TWebKitDOMHTMLDocumentClass;
  TWebKitDOMHTMLDocumentClass = object
    parent_class: TWebKitDOMDocumentClass;
  end;

  PPWebKitDOMHTMLEmbedElement = ^PWebKitDOMHTMLEmbedElement;
  PWebKitDOMHTMLEmbedElement = ^TWebKitDOMHTMLEmbedElement;
  TWebKitDOMHTMLEmbedElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    function get_height: glong; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_src: Pgchar; cdecl; inline;
    function get_width: glong; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_height(value: glong); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_src(value: Pgchar); cdecl; inline;
    procedure set_width(value: glong); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property height:  glong read get_height  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property src:  Pgchar read get_src  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
    property width:  glong read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLEmbedElementClass = ^PWebKitDOMHTMLEmbedElementClass;
  PWebKitDOMHTMLEmbedElementClass = ^TWebKitDOMHTMLEmbedElementClass;
  TWebKitDOMHTMLEmbedElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLFieldSetElement = ^PWebKitDOMHTMLFieldSetElement;
  PWebKitDOMHTMLFieldSetElement = ^TWebKitDOMHTMLFieldSetElement;
  TWebKitDOMHTMLFieldSetElement = object(TWebKitDOMHTMLElement)
    function check_validity: gboolean; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_validation_message: Pgchar; cdecl; inline;
    function get_validity: PWebKitDOMValidityState; cdecl; inline;
    function get_will_validate: gboolean; cdecl; inline;
    procedure set_custom_validity(error: Pgchar); cdecl; inline;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property validation_message:  Pgchar read get_validation_message ;
    property validity:  PWebKitDOMValidityState read get_validity ;
    property will_validate:  gboolean read get_will_validate ;
  end;

  PPWebKitDOMHTMLFieldSetElementClass = ^PWebKitDOMHTMLFieldSetElementClass;
  PWebKitDOMHTMLFieldSetElementClass = ^TWebKitDOMHTMLFieldSetElementClass;
  TWebKitDOMHTMLFieldSetElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLFontElement = ^PWebKitDOMHTMLFontElement;
  PWebKitDOMHTMLFontElement = ^TWebKitDOMHTMLFontElement;
  TWebKitDOMHTMLFontElement = object(TWebKitDOMHTMLElement)
    function get_color: Pgchar; cdecl; inline;
    function get_face: Pgchar; cdecl; inline;
    function get_size: Pgchar; cdecl; inline;
    procedure set_color(value: Pgchar); cdecl; inline;
    procedure set_face(value: Pgchar); cdecl; inline;
    procedure set_size(value: Pgchar); cdecl; inline;
    property color:  Pgchar read get_color  { property is writeable but setter not declared } ;
    property face:  Pgchar read get_face  { property is writeable but setter not declared } ;
    property size:  Pgchar read get_size  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLFontElementClass = ^PWebKitDOMHTMLFontElementClass;
  PWebKitDOMHTMLFontElementClass = ^TWebKitDOMHTMLFontElementClass;
  TWebKitDOMHTMLFontElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLFormElementClass = ^PWebKitDOMHTMLFormElementClass;
  PWebKitDOMHTMLFormElementClass = ^TWebKitDOMHTMLFormElementClass;
  TWebKitDOMHTMLFormElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLFrameElement = ^PWebKitDOMHTMLFrameElement;
  PWebKitDOMHTMLFrameElement = ^TWebKitDOMHTMLFrameElement;
  TWebKitDOMHTMLFrameElement = object(TWebKitDOMHTMLElement)
    function get_content_document: PWebKitDOMDocument; cdecl; inline;
    function get_content_window: PWebKitDOMDOMWindow; cdecl; inline;
    function get_frame_border: Pgchar; cdecl; inline;
    function get_height: glong; cdecl; inline;
    function get_long_desc: Pgchar; cdecl; inline;
    function get_margin_height: Pgchar; cdecl; inline;
    function get_margin_width: Pgchar; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_no_resize: gboolean; cdecl; inline;
    function get_scrolling: Pgchar; cdecl; inline;
    function get_src: Pgchar; cdecl; inline;
    function get_width: glong; cdecl; inline;
    procedure set_frame_border(value: Pgchar); cdecl; inline;
    procedure set_long_desc(value: Pgchar); cdecl; inline;
    procedure set_margin_height(value: Pgchar); cdecl; inline;
    procedure set_margin_width(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_no_resize(value: gboolean); cdecl; inline;
    procedure set_scrolling(value: Pgchar); cdecl; inline;
    procedure set_src(value: Pgchar); cdecl; inline;
    property content_document:  PWebKitDOMDocument read get_content_document ;
    property content_window:  PWebKitDOMDOMWindow read get_content_window ;
    property frame_border:  Pgchar read get_frame_border  { property is writeable but setter not declared } ;
    property height:  glong read get_height ;
    property long_desc:  Pgchar read get_long_desc  { property is writeable but setter not declared } ;
    property margin_height:  Pgchar read get_margin_height  { property is writeable but setter not declared } ;
    property margin_width:  Pgchar read get_margin_width  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property no_resize:  gboolean read get_no_resize  { property is writeable but setter not declared } ;
    property scrolling:  Pgchar read get_scrolling  { property is writeable but setter not declared } ;
    property src:  Pgchar read get_src  { property is writeable but setter not declared } ;
    property width:  glong read get_width ;
  end;

  PPWebKitDOMHTMLFrameElementClass = ^PWebKitDOMHTMLFrameElementClass;
  PWebKitDOMHTMLFrameElementClass = ^TWebKitDOMHTMLFrameElementClass;
  TWebKitDOMHTMLFrameElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLFrameSetElement = ^PWebKitDOMHTMLFrameSetElement;
  PWebKitDOMHTMLFrameSetElement = ^TWebKitDOMHTMLFrameSetElement;
  TWebKitDOMHTMLFrameSetElement = object(TWebKitDOMHTMLElement)
    function get_cols: Pgchar; cdecl; inline;
    function get_rows: Pgchar; cdecl; inline;
    procedure set_cols(value: Pgchar); cdecl; inline;
    procedure set_rows(value: Pgchar); cdecl; inline;
    property cols:  Pgchar read get_cols  { property is writeable but setter not declared } ;
    property rows:  Pgchar read get_rows  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLFrameSetElementClass = ^PWebKitDOMHTMLFrameSetElementClass;
  PWebKitDOMHTMLFrameSetElementClass = ^TWebKitDOMHTMLFrameSetElementClass;
  TWebKitDOMHTMLFrameSetElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLHRElement = ^PWebKitDOMHTMLHRElement;
  PWebKitDOMHTMLHRElement = ^TWebKitDOMHTMLHRElement;
  TWebKitDOMHTMLHRElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    function get_no_shade: gboolean; cdecl; inline;
    function get_size: Pgchar; cdecl; inline;
    function get_width: Pgchar; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_no_shade(value: gboolean); cdecl; inline;
    procedure set_size(value: Pgchar); cdecl; inline;
    procedure set_width(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property no_shade:  gboolean read get_no_shade  { property is writeable but setter not declared } ;
    property size:  Pgchar read get_size  { property is writeable but setter not declared } ;
    property width:  Pgchar read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLHRElementClass = ^PWebKitDOMHTMLHRElementClass;
  PWebKitDOMHTMLHRElementClass = ^TWebKitDOMHTMLHRElementClass;
  TWebKitDOMHTMLHRElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLHeadElementClass = ^PWebKitDOMHTMLHeadElementClass;
  PWebKitDOMHTMLHeadElementClass = ^TWebKitDOMHTMLHeadElementClass;
  TWebKitDOMHTMLHeadElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLHeadingElement = ^PWebKitDOMHTMLHeadingElement;
  PWebKitDOMHTMLHeadingElement = ^TWebKitDOMHTMLHeadingElement;
  TWebKitDOMHTMLHeadingElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLHeadingElementClass = ^PWebKitDOMHTMLHeadingElementClass;
  PWebKitDOMHTMLHeadingElementClass = ^TWebKitDOMHTMLHeadingElementClass;
  TWebKitDOMHTMLHeadingElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLHtmlElement = ^PWebKitDOMHTMLHtmlElement;
  PWebKitDOMHTMLHtmlElement = ^TWebKitDOMHTMLHtmlElement;
  TWebKitDOMHTMLHtmlElement = object(TWebKitDOMHTMLElement)
    function get_manifest: Pgchar; cdecl; inline;
    function get_version: Pgchar; cdecl; inline;
    procedure set_manifest(value: Pgchar); cdecl; inline;
    procedure set_version(value: Pgchar); cdecl; inline;
    property manifest:  Pgchar read get_manifest  { property is writeable but setter not declared } ;
    property version:  Pgchar read get_version  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLHtmlElementClass = ^PWebKitDOMHTMLHtmlElementClass;
  PWebKitDOMHTMLHtmlElementClass = ^TWebKitDOMHTMLHtmlElementClass;
  TWebKitDOMHTMLHtmlElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLIFrameElement = ^PWebKitDOMHTMLIFrameElement;
  PWebKitDOMHTMLIFrameElement = ^TWebKitDOMHTMLIFrameElement;
  TWebKitDOMHTMLIFrameElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    function get_content_document: PWebKitDOMDocument; cdecl; inline;
    function get_content_window: PWebKitDOMDOMWindow; cdecl; inline;
    function get_frame_border: Pgchar; cdecl; inline;
    function get_height: Pgchar; cdecl; inline;
    function get_long_desc: Pgchar; cdecl; inline;
    function get_margin_height: Pgchar; cdecl; inline;
    function get_margin_width: Pgchar; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_sandbox: Pgchar; cdecl; inline;
    function get_scrolling: Pgchar; cdecl; inline;
    function get_src: Pgchar; cdecl; inline;
    function get_width: Pgchar; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_frame_border(value: Pgchar); cdecl; inline;
    procedure set_height(value: Pgchar); cdecl; inline;
    procedure set_long_desc(value: Pgchar); cdecl; inline;
    procedure set_margin_height(value: Pgchar); cdecl; inline;
    procedure set_margin_width(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_sandbox(value: Pgchar); cdecl; inline;
    procedure set_scrolling(value: Pgchar); cdecl; inline;
    procedure set_src(value: Pgchar); cdecl; inline;
    procedure set_width(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property content_document:  PWebKitDOMDocument read get_content_document ;
    property content_window:  PWebKitDOMDOMWindow read get_content_window ;
    property frame_border:  Pgchar read get_frame_border  { property is writeable but setter not declared } ;
    property height:  Pgchar read get_height  { property is writeable but setter not declared } ;
    property long_desc:  Pgchar read get_long_desc  { property is writeable but setter not declared } ;
    property margin_height:  Pgchar read get_margin_height  { property is writeable but setter not declared } ;
    property margin_width:  Pgchar read get_margin_width  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property sandbox:  Pgchar read get_sandbox  { property is writeable but setter not declared } ;
    property scrolling:  Pgchar read get_scrolling  { property is writeable but setter not declared } ;
    property src:  Pgchar read get_src  { property is writeable but setter not declared } ;
    property width:  Pgchar read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLIFrameElementClass = ^PWebKitDOMHTMLIFrameElementClass;
  PWebKitDOMHTMLIFrameElementClass = ^TWebKitDOMHTMLIFrameElementClass;
  TWebKitDOMHTMLIFrameElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLImageElement = ^PWebKitDOMHTMLImageElement;
  PWebKitDOMHTMLImageElement = ^TWebKitDOMHTMLImageElement;
  TWebKitDOMHTMLImageElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    function get_alt: Pgchar; cdecl; inline;
    function get_border: Pgchar; cdecl; inline;
    function get_complete: gboolean; cdecl; inline;
    function get_height: glong; cdecl; inline;
    function get_hspace: glong; cdecl; inline;
    function get_is_map: gboolean; cdecl; inline;
    function get_long_desc: Pgchar; cdecl; inline;
    function get_lowsrc: Pgchar; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_natural_height: glong; cdecl; inline;
    function get_natural_width: glong; cdecl; inline;
    function get_src: Pgchar; cdecl; inline;
    function get_use_map: Pgchar; cdecl; inline;
    function get_vspace: glong; cdecl; inline;
    function get_width: glong; cdecl; inline;
    function get_x: glong; cdecl; inline;
    function get_y: glong; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_alt(value: Pgchar); cdecl; inline;
    procedure set_border(value: Pgchar); cdecl; inline;
    procedure set_height(value: glong); cdecl; inline;
    procedure set_hspace(value: glong); cdecl; inline;
    procedure set_is_map(value: gboolean); cdecl; inline;
    procedure set_long_desc(value: Pgchar); cdecl; inline;
    procedure set_lowsrc(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_src(value: Pgchar); cdecl; inline;
    procedure set_use_map(value: Pgchar); cdecl; inline;
    procedure set_vspace(value: glong); cdecl; inline;
    procedure set_width(value: glong); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property alt:  Pgchar read get_alt  { property is writeable but setter not declared } ;
    property border:  Pgchar read get_border  { property is writeable but setter not declared } ;
    property complete:  gboolean read get_complete ;
    property height:  glong read get_height  { property is writeable but setter not declared } ;
    property hspace:  glong read get_hspace  { property is writeable but setter not declared } ;
    property is_map:  gboolean read get_is_map  { property is writeable but setter not declared } ;
    property long_desc:  Pgchar read get_long_desc  { property is writeable but setter not declared } ;
    property lowsrc:  Pgchar read get_lowsrc  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property natural_height:  glong read get_natural_height ;
    property natural_width:  glong read get_natural_width ;
    property src:  Pgchar read get_src  { property is writeable but setter not declared } ;
    property use_map:  Pgchar read get_use_map  { property is writeable but setter not declared } ;
    property vspace:  glong read get_vspace  { property is writeable but setter not declared } ;
    property width:  glong read get_width  { property is writeable but setter not declared } ;
    property x:  glong read get_x ;
    property y:  glong read get_y ;
  end;

  PPWebKitDOMHTMLImageElementClass = ^PWebKitDOMHTMLImageElementClass;
  PWebKitDOMHTMLImageElementClass = ^TWebKitDOMHTMLImageElementClass;
  TWebKitDOMHTMLImageElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLOptionElement = ^PWebKitDOMHTMLOptionElement;
  PWebKitDOMHTMLOptionElement = ^TWebKitDOMHTMLOptionElement;
  TWebKitDOMHTMLOptionElement = object(TWebKitDOMHTMLElement)
    function get_default_selected: gboolean; cdecl; inline;
    function get_disabled: gboolean; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_index: glong; cdecl; inline;
    function get_label: Pgchar; cdecl; inline;
    function get_selected: gboolean; cdecl; inline;
    function get_text: Pgchar; cdecl; inline;
    function get_value: Pgchar; cdecl; inline;
    procedure set_default_selected(value: gboolean); cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_label(value: Pgchar); cdecl; inline;
    procedure set_selected(value: gboolean); cdecl; inline;
    procedure set_value(value: Pgchar); cdecl; inline;
    property default_selected:  gboolean read get_default_selected  { property is writeable but setter not declared } ;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property index:  glong read get_index ;
    property label_:  Pgchar read get_label  { property is writeable but setter not declared } ;
    property selected:  gboolean read get_selected  { property is writeable but setter not declared } ;
    property text:  Pgchar read get_text ;
    property value:  Pgchar read get_value  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLInputElement = ^PWebKitDOMHTMLInputElement;
  PWebKitDOMHTMLInputElement = ^TWebKitDOMHTMLInputElement;
  TWebKitDOMHTMLInputElement = object(TWebKitDOMHTMLElement)
    function check_validity: gboolean; cdecl; inline;
    procedure click; cdecl; inline;
    function get_accept: Pgchar; cdecl; inline;
    function get_access_key: Pgchar; cdecl; inline;
    function get_align: Pgchar; cdecl; inline;
    function get_alt: Pgchar; cdecl; inline;
    function get_autofocus: gboolean; cdecl; inline;
    function get_checked: gboolean; cdecl; inline;
    function get_default_checked: gboolean; cdecl; inline;
    function get_default_value: Pgchar; cdecl; inline;
    function get_disabled: gboolean; cdecl; inline;
    function get_files: PWebKitDOMFileList; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_form_action: Pgchar; cdecl; inline;
    function get_form_enctype: Pgchar; cdecl; inline;
    function get_form_method: Pgchar; cdecl; inline;
    function get_form_no_validate: gboolean; cdecl; inline;
    function get_form_target: Pgchar; cdecl; inline;
    function get_incremental: gboolean; cdecl; inline;
    function get_indeterminate: gboolean; cdecl; inline;
    function get_labels: PWebKitDOMNodeList; cdecl; inline;
    function get_list: PWebKitDOMHTMLElement; cdecl; inline;
    function get_max: Pgchar; cdecl; inline;
    function get_max_length: glong; cdecl; inline;
    function get_min: Pgchar; cdecl; inline;
    function get_multiple: gboolean; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_pattern: Pgchar; cdecl; inline;
    function get_placeholder: Pgchar; cdecl; inline;
    function get_read_only: gboolean; cdecl; inline;
    function get_required: gboolean; cdecl; inline;
    function get_selected_option: PWebKitDOMHTMLOptionElement; cdecl; inline;
    function get_size: gulong; cdecl; inline;
    function get_src: Pgchar; cdecl; inline;
    function get_step: Pgchar; cdecl; inline;
    function get_use_map: Pgchar; cdecl; inline;
    function get_validation_message: Pgchar; cdecl; inline;
    function get_validity: PWebKitDOMValidityState; cdecl; inline;
    function get_value: Pgchar; cdecl; inline;
    function get_value_as_number: gdouble; cdecl; inline;
    function get_will_validate: gboolean; cdecl; inline;
    procedure select; cdecl; inline;
    procedure set_accept(value: Pgchar); cdecl; inline;
    procedure set_access_key(value: Pgchar); cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_alt(value: Pgchar); cdecl; inline;
    procedure set_autofocus(value: gboolean); cdecl; inline;
    procedure set_checked(value: gboolean); cdecl; inline;
    procedure set_custom_validity(error: Pgchar); cdecl; inline;
    procedure set_default_checked(value: gboolean); cdecl; inline;
    procedure set_default_value(value: Pgchar); cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_form_action(value: Pgchar); cdecl; inline;
    procedure set_form_enctype(value: Pgchar); cdecl; inline;
    procedure set_form_method(value: Pgchar); cdecl; inline;
    procedure set_form_no_validate(value: gboolean); cdecl; inline;
    procedure set_form_target(value: Pgchar); cdecl; inline;
    procedure set_incremental(value: gboolean); cdecl; inline;
    procedure set_indeterminate(value: gboolean); cdecl; inline;
    procedure set_max(value: Pgchar); cdecl; inline;
    procedure set_max_length(value: glong); cdecl; inline;
    procedure set_min(value: Pgchar); cdecl; inline;
    procedure set_multiple(value: gboolean); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_pattern(value: Pgchar); cdecl; inline;
    procedure set_placeholder(value: Pgchar); cdecl; inline;
    procedure set_read_only(value: gboolean); cdecl; inline;
    procedure set_required(value: gboolean); cdecl; inline;
    procedure set_size(value: gulong); cdecl; inline;
    procedure set_src(value: Pgchar); cdecl; inline;
    procedure set_step(value: Pgchar); cdecl; inline;
    procedure set_use_map(value: Pgchar); cdecl; inline;
    procedure set_value(value: Pgchar); cdecl; inline;
    procedure set_value_as_number(value: gdouble); cdecl; inline;
    procedure set_value_for_user(value: Pgchar); cdecl; inline;
    procedure step_down(n: glong); cdecl; inline;
    procedure step_up(n: glong); cdecl; inline;
    property accept:  Pgchar read get_accept  { property is writeable but setter not declared } ;
    property access_key:  Pgchar read get_access_key  { property is writeable but setter not declared } ;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property alt:  Pgchar read get_alt  { property is writeable but setter not declared } ;
    property autofocus:  gboolean read get_autofocus  { property is writeable but setter not declared } ;
    property checked:  gboolean read get_checked  { property is writeable but setter not declared } ;
    property default_checked:  gboolean read get_default_checked  { property is writeable but setter not declared } ;
    property default_value:  Pgchar read get_default_value  { property is writeable but setter not declared } ;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property files:  PWebKitDOMFileList read get_files ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property form_action:  Pgchar read get_form_action  { property is writeable but setter not declared } ;
    property form_enctype:  Pgchar read get_form_enctype  { property is writeable but setter not declared } ;
    property form_method:  Pgchar read get_form_method  { property is writeable but setter not declared } ;
    property form_no_validate:  gboolean read get_form_no_validate  { property is writeable but setter not declared } ;
    property form_target:  Pgchar read get_form_target  { property is writeable but setter not declared } ;
    property incremental:  gboolean read get_incremental  { property is writeable but setter not declared } ;
    property indeterminate:  gboolean read get_indeterminate  { property is writeable but setter not declared } ;
    property labels:  PWebKitDOMNodeList read get_labels ;
    property list:  PWebKitDOMHTMLElement read get_list ;
    property max:  Pgchar read get_max  { property is writeable but setter not declared } ;
    property max_length:  glong read get_max_length  { property is writeable but setter not declared } ;
    property min:  Pgchar read get_min  { property is writeable but setter not declared } ;
    property multiple:  gboolean read get_multiple  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property pattern:  Pgchar read get_pattern  { property is writeable but setter not declared } ;
    property placeholder:  Pgchar read get_placeholder  { property is writeable but setter not declared } ;
    property read_only:  gboolean read get_read_only  { property is writeable but setter not declared } ;
    property required:  gboolean read get_required  { property is writeable but setter not declared } ;
    property selected_option:  PWebKitDOMHTMLOptionElement read get_selected_option ;
    property size:  gulong read get_size  { property is writeable but setter not declared } ;
    property src:  Pgchar read get_src  { property is writeable but setter not declared } ;
    property step:  Pgchar read get_step  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
    property use_map:  Pgchar read get_use_map  { property is writeable but setter not declared } ;
    property validation_message:  Pgchar read get_validation_message ;
    property validity:  PWebKitDOMValidityState read get_validity ;
    property value:  Pgchar read get_value  { property is writeable but setter not declared } ;
    property value_as_number:  gdouble read get_value_as_number  { property is writeable but setter not declared } ;
    property will_validate:  gboolean read get_will_validate ;
  end;

  PPWebKitDOMHTMLInputElementClass = ^PWebKitDOMHTMLInputElementClass;
  PWebKitDOMHTMLInputElementClass = ^TWebKitDOMHTMLInputElementClass;
  TWebKitDOMHTMLInputElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLIsIndexElement = ^PWebKitDOMHTMLIsIndexElement;
  PWebKitDOMHTMLIsIndexElement = ^TWebKitDOMHTMLIsIndexElement;
  TWebKitDOMHTMLIsIndexElement = object(TWebKitDOMHTMLInputElement)
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_prompt: Pgchar; cdecl; inline;
    procedure set_prompt(value: Pgchar); cdecl; inline;
    property form1:  PWebKitDOMHTMLFormElement read get_form ;
    property prompt:  Pgchar read get_prompt  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLIsIndexElementClass = ^PWebKitDOMHTMLIsIndexElementClass;
  PWebKitDOMHTMLIsIndexElementClass = ^TWebKitDOMHTMLIsIndexElementClass;
  TWebKitDOMHTMLIsIndexElementClass = object
    parent_class: TWebKitDOMHTMLInputElementClass;
  end;

  PPWebKitDOMHTMLKeygenElement = ^PWebKitDOMHTMLKeygenElement;
  PWebKitDOMHTMLKeygenElement = ^TWebKitDOMHTMLKeygenElement;
  TWebKitDOMHTMLKeygenElement = object(TWebKitDOMHTMLElement)
    function check_validity: gboolean; cdecl; inline;
    function get_autofocus: gboolean; cdecl; inline;
    function get_challenge: Pgchar; cdecl; inline;
    function get_disabled: gboolean; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_keytype: Pgchar; cdecl; inline;
    function get_labels: PWebKitDOMNodeList; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_validation_message: Pgchar; cdecl; inline;
    function get_validity: PWebKitDOMValidityState; cdecl; inline;
    function get_will_validate: gboolean; cdecl; inline;
    procedure set_autofocus(value: gboolean); cdecl; inline;
    procedure set_challenge(value: Pgchar); cdecl; inline;
    procedure set_custom_validity(error: Pgchar); cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_keytype(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    property autofocus:  gboolean read get_autofocus  { property is writeable but setter not declared } ;
    property challenge:  Pgchar read get_challenge  { property is writeable but setter not declared } ;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property keytype:  Pgchar read get_keytype  { property is writeable but setter not declared } ;
    property labels:  PWebKitDOMNodeList read get_labels ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
    property validation_message:  Pgchar read get_validation_message ;
    property validity:  PWebKitDOMValidityState read get_validity ;
    property will_validate:  gboolean read get_will_validate ;
  end;

  PPWebKitDOMHTMLKeygenElementClass = ^PWebKitDOMHTMLKeygenElementClass;
  PWebKitDOMHTMLKeygenElementClass = ^TWebKitDOMHTMLKeygenElementClass;
  TWebKitDOMHTMLKeygenElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLLIElement = ^PWebKitDOMHTMLLIElement;
  PWebKitDOMHTMLLIElement = ^TWebKitDOMHTMLLIElement;
  TWebKitDOMHTMLLIElement = object(TWebKitDOMHTMLElement)
    function get_value: glong; cdecl; inline;
    procedure set_value(value: glong); cdecl; inline;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
    property value:  glong read get_value  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLLIElementClass = ^PWebKitDOMHTMLLIElementClass;
  PWebKitDOMHTMLLIElementClass = ^TWebKitDOMHTMLLIElementClass;
  TWebKitDOMHTMLLIElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLLabelElement = ^PWebKitDOMHTMLLabelElement;
  PWebKitDOMHTMLLabelElement = ^TWebKitDOMHTMLLabelElement;
  TWebKitDOMHTMLLabelElement = object(TWebKitDOMHTMLElement)
    function get_access_key: Pgchar; cdecl; inline;
    function get_control: PWebKitDOMHTMLElement; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_html_for: Pgchar; cdecl; inline;
    procedure set_access_key(value: Pgchar); cdecl; inline;
    procedure set_html_for(value: Pgchar); cdecl; inline;
    property access_key:  Pgchar read get_access_key  { property is writeable but setter not declared } ;
    property control:  PWebKitDOMHTMLElement read get_control ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property html_for:  Pgchar read get_html_for  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLLabelElementClass = ^PWebKitDOMHTMLLabelElementClass;
  PWebKitDOMHTMLLabelElementClass = ^TWebKitDOMHTMLLabelElementClass;
  TWebKitDOMHTMLLabelElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLLegendElement = ^PWebKitDOMHTMLLegendElement;
  PWebKitDOMHTMLLegendElement = ^TWebKitDOMHTMLLegendElement;
  TWebKitDOMHTMLLegendElement = object(TWebKitDOMHTMLElement)
    function get_access_key: Pgchar; cdecl; inline;
    function get_align: Pgchar; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    procedure set_access_key(value: Pgchar); cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    property access_key:  Pgchar read get_access_key  { property is writeable but setter not declared } ;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
  end;

  PPWebKitDOMHTMLLegendElementClass = ^PWebKitDOMHTMLLegendElementClass;
  PWebKitDOMHTMLLegendElementClass = ^TWebKitDOMHTMLLegendElementClass;
  TWebKitDOMHTMLLegendElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLLinkElement = ^PWebKitDOMHTMLLinkElement;
  PWebKitDOMHTMLLinkElement = ^TWebKitDOMHTMLLinkElement;
  TWebKitDOMHTMLLinkElement = object(TWebKitDOMHTMLElement)
    function get_charset: Pgchar; cdecl; inline;
    function get_disabled: gboolean; cdecl; inline;
    function get_href: Pgchar; cdecl; inline;
    function get_hreflang: Pgchar; cdecl; inline;
    function get_media: Pgchar; cdecl; inline;
    function get_rel: Pgchar; cdecl; inline;
    function get_rev: Pgchar; cdecl; inline;
    function get_sheet: PWebKitDOMStyleSheet; cdecl; inline;
    function get_target: Pgchar; cdecl; inline;
    procedure set_charset(value: Pgchar); cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_href(value: Pgchar); cdecl; inline;
    procedure set_hreflang(value: Pgchar); cdecl; inline;
    procedure set_media(value: Pgchar); cdecl; inline;
    procedure set_rel(value: Pgchar); cdecl; inline;
    procedure set_rev(value: Pgchar); cdecl; inline;
    procedure set_target(value: Pgchar); cdecl; inline;
    property charset:  Pgchar read get_charset  { property is writeable but setter not declared } ;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property href:  Pgchar read get_href  { property is writeable but setter not declared } ;
    property hreflang:  Pgchar read get_hreflang  { property is writeable but setter not declared } ;
    property media:  Pgchar read get_media  { property is writeable but setter not declared } ;
    property rel:  Pgchar read get_rel  { property is writeable but setter not declared } ;
    property rev:  Pgchar read get_rev  { property is writeable but setter not declared } ;
    property sheet:  PWebKitDOMStyleSheet read get_sheet ;
    property target:  Pgchar read get_target  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLLinkElementClass = ^PWebKitDOMHTMLLinkElementClass;
  PWebKitDOMHTMLLinkElementClass = ^TWebKitDOMHTMLLinkElementClass;
  TWebKitDOMHTMLLinkElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLMapElement = ^PWebKitDOMHTMLMapElement;
  PWebKitDOMHTMLMapElement = ^TWebKitDOMHTMLMapElement;
  TWebKitDOMHTMLMapElement = object(TWebKitDOMHTMLElement)
    function get_areas: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    property areas:  PWebKitDOMHTMLCollection read get_areas ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLMapElementClass = ^PWebKitDOMHTMLMapElementClass;
  PWebKitDOMHTMLMapElementClass = ^TWebKitDOMHTMLMapElementClass;
  TWebKitDOMHTMLMapElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLMarqueeElement = ^PWebKitDOMHTMLMarqueeElement;
  PWebKitDOMHTMLMarqueeElement = ^TWebKitDOMHTMLMarqueeElement;
  TWebKitDOMHTMLMarqueeElement = object(TWebKitDOMHTMLElement)
    function get_behavior: Pgchar; cdecl; inline;
    function get_bg_color: Pgchar; cdecl; inline;
    function get_direction: Pgchar; cdecl; inline;
    function get_height: Pgchar; cdecl; inline;
    function get_hspace: gulong; cdecl; inline;
    function get_loop: glong; cdecl; inline;
    function get_scroll_amount: glong; cdecl; inline;
    function get_scroll_delay: glong; cdecl; inline;
    function get_true_speed: gboolean; cdecl; inline;
    function get_vspace: gulong; cdecl; inline;
    function get_width: Pgchar; cdecl; inline;
    procedure set_behavior(value: Pgchar); cdecl; inline;
    procedure set_bg_color(value: Pgchar); cdecl; inline;
    procedure set_direction(value: Pgchar); cdecl; inline;
    procedure set_height(value: Pgchar); cdecl; inline;
    procedure set_hspace(value: gulong); cdecl; inline;
    procedure set_loop(value: glong); cdecl; inline;
    procedure set_scroll_amount(value: glong); cdecl; inline;
    procedure set_scroll_delay(value: glong); cdecl; inline;
    procedure set_true_speed(value: gboolean); cdecl; inline;
    procedure set_vspace(value: gulong); cdecl; inline;
    procedure set_width(value: Pgchar); cdecl; inline;
    procedure start; cdecl; inline;
    procedure stop; cdecl; inline;
    property behavior:  Pgchar read get_behavior  { property is writeable but setter not declared } ;
    property bg_color:  Pgchar read get_bg_color  { property is writeable but setter not declared } ;
    property direction:  Pgchar read get_direction  { property is writeable but setter not declared } ;
    property height:  Pgchar read get_height  { property is writeable but setter not declared } ;
    property hspace:  gulong read get_hspace  { property is writeable but setter not declared } ;
    property loop:  glong read get_loop  { property is writeable but setter not declared } ;
    property scroll_amount:  glong read get_scroll_amount  { property is writeable but setter not declared } ;
    property scroll_delay:  glong read get_scroll_delay  { property is writeable but setter not declared } ;
    property true_speed:  gboolean read get_true_speed  { property is writeable but setter not declared } ;
    property vspace:  gulong read get_vspace  { property is writeable but setter not declared } ;
    property width:  Pgchar read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLMarqueeElementClass = ^PWebKitDOMHTMLMarqueeElementClass;
  PWebKitDOMHTMLMarqueeElementClass = ^TWebKitDOMHTMLMarqueeElementClass;
  TWebKitDOMHTMLMarqueeElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;
  TWebKitDOMTimeRanges = object(TWebKitDOMObject)
    function end_(index: gulong): gfloat; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    function start(index: gulong): gfloat; cdecl; inline;
    property length:  gulong read get_length ;
  end;
  TWebKitDOMMediaError = object(TWebKitDOMObject)
    function get_code: gushort; cdecl; inline;
    property code:  gushort read get_code ;
  end;

  PPWebKitDOMHTMLMenuElement = ^PWebKitDOMHTMLMenuElement;
  PWebKitDOMHTMLMenuElement = ^TWebKitDOMHTMLMenuElement;
  TWebKitDOMHTMLMenuElement = object(TWebKitDOMHTMLElement)
    function get_compact: gboolean; cdecl; inline;
    procedure set_compact(value: gboolean); cdecl; inline;
    property compact:  gboolean read get_compact  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLMenuElementClass = ^PWebKitDOMHTMLMenuElementClass;
  PWebKitDOMHTMLMenuElementClass = ^TWebKitDOMHTMLMenuElementClass;
  TWebKitDOMHTMLMenuElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLMetaElement = ^PWebKitDOMHTMLMetaElement;
  PWebKitDOMHTMLMetaElement = ^TWebKitDOMHTMLMetaElement;
  TWebKitDOMHTMLMetaElement = object(TWebKitDOMHTMLElement)
    function get_content: Pgchar; cdecl; inline;
    function get_http_equiv: Pgchar; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_scheme: Pgchar; cdecl; inline;
    procedure set_content(value: Pgchar); cdecl; inline;
    procedure set_http_equiv(value: Pgchar); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_scheme(value: Pgchar); cdecl; inline;
    property content:  Pgchar read get_content  { property is writeable but setter not declared } ;
    property http_equiv:  Pgchar read get_http_equiv  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property scheme:  Pgchar read get_scheme  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLMetaElementClass = ^PWebKitDOMHTMLMetaElementClass;
  PWebKitDOMHTMLMetaElementClass = ^TWebKitDOMHTMLMetaElementClass;
  TWebKitDOMHTMLMetaElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLModElement = ^PWebKitDOMHTMLModElement;
  PWebKitDOMHTMLModElement = ^TWebKitDOMHTMLModElement;
  TWebKitDOMHTMLModElement = object(TWebKitDOMHTMLElement)
    function get_cite: Pgchar; cdecl; inline;
    function get_date_time: Pgchar; cdecl; inline;
    procedure set_cite(value: Pgchar); cdecl; inline;
    procedure set_date_time(value: Pgchar); cdecl; inline;
    property cite:  Pgchar read get_cite  { property is writeable but setter not declared } ;
    property date_time:  Pgchar read get_date_time  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLModElementClass = ^PWebKitDOMHTMLModElementClass;
  PWebKitDOMHTMLModElementClass = ^TWebKitDOMHTMLModElementClass;
  TWebKitDOMHTMLModElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLOListElement = ^PWebKitDOMHTMLOListElement;
  PWebKitDOMHTMLOListElement = ^TWebKitDOMHTMLOListElement;
  TWebKitDOMHTMLOListElement = object(TWebKitDOMHTMLElement)
    function get_compact: gboolean; cdecl; inline;
    function get_start: glong; cdecl; inline;
    procedure set_compact(value: gboolean); cdecl; inline;
    procedure set_start(value: glong); cdecl; inline;
    property compact:  gboolean read get_compact  { property is writeable but setter not declared } ;
    property start:  glong read get_start  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLOListElementClass = ^PWebKitDOMHTMLOListElementClass;
  PWebKitDOMHTMLOListElementClass = ^TWebKitDOMHTMLOListElementClass;
  TWebKitDOMHTMLOListElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLObjectElement = ^PWebKitDOMHTMLObjectElement;
  PWebKitDOMHTMLObjectElement = ^TWebKitDOMHTMLObjectElement;
  TWebKitDOMHTMLObjectElement = object(TWebKitDOMHTMLElement)
    function check_validity: gboolean; cdecl; inline;
    function get_align: Pgchar; cdecl; inline;
    function get_archive: Pgchar; cdecl; inline;
    function get_border: Pgchar; cdecl; inline;
    function get_code: Pgchar; cdecl; inline;
    function get_code_base: Pgchar; cdecl; inline;
    function get_code_type: Pgchar; cdecl; inline;
    function get_content_document: PWebKitDOMDocument; cdecl; inline;
    function get_data: Pgchar; cdecl; inline;
    function get_declare: gboolean; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_height: Pgchar; cdecl; inline;
    function get_hspace: glong; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_standby: Pgchar; cdecl; inline;
    function get_use_map: Pgchar; cdecl; inline;
    function get_validation_message: Pgchar; cdecl; inline;
    function get_validity: PWebKitDOMValidityState; cdecl; inline;
    function get_vspace: glong; cdecl; inline;
    function get_width: Pgchar; cdecl; inline;
    function get_will_validate: gboolean; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_archive(value: Pgchar); cdecl; inline;
    procedure set_border(value: Pgchar); cdecl; inline;
    procedure set_code(value: Pgchar); cdecl; inline;
    procedure set_code_base(value: Pgchar); cdecl; inline;
    procedure set_code_type(value: Pgchar); cdecl; inline;
    procedure set_custom_validity(error: Pgchar); cdecl; inline;
    procedure set_data(value: Pgchar); cdecl; inline;
    procedure set_declare(value: gboolean); cdecl; inline;
    procedure set_height(value: Pgchar); cdecl; inline;
    procedure set_hspace(value: glong); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_standby(value: Pgchar); cdecl; inline;
    procedure set_use_map(value: Pgchar); cdecl; inline;
    procedure set_vspace(value: glong); cdecl; inline;
    procedure set_width(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property archive:  Pgchar read get_archive  { property is writeable but setter not declared } ;
    property border:  Pgchar read get_border  { property is writeable but setter not declared } ;
    property code:  Pgchar read get_code  { property is writeable but setter not declared } ;
    property code_base:  Pgchar read get_code_base  { property is writeable but setter not declared } ;
    property code_type:  Pgchar read get_code_type  { property is writeable but setter not declared } ;
    property content_document:  PWebKitDOMDocument read get_content_document ;
    property data:  Pgchar read get_data  { property is writeable but setter not declared } ;
    property declare:  gboolean read get_declare  { property is writeable but setter not declared } ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property height:  Pgchar read get_height  { property is writeable but setter not declared } ;
    property hspace:  glong read get_hspace  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property standby:  Pgchar read get_standby  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
    property use_map:  Pgchar read get_use_map  { property is writeable but setter not declared } ;
    property validation_message:  Pgchar read get_validation_message ;
    property validity:  PWebKitDOMValidityState read get_validity ;
    property vspace:  glong read get_vspace  { property is writeable but setter not declared } ;
    property width:  Pgchar read get_width  { property is writeable but setter not declared } ;
    property will_validate:  gboolean read get_will_validate ;
  end;

  PPWebKitDOMHTMLObjectElementClass = ^PWebKitDOMHTMLObjectElementClass;
  PWebKitDOMHTMLObjectElementClass = ^TWebKitDOMHTMLObjectElementClass;
  TWebKitDOMHTMLObjectElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLOptGroupElement = ^PWebKitDOMHTMLOptGroupElement;
  PWebKitDOMHTMLOptGroupElement = ^TWebKitDOMHTMLOptGroupElement;
  TWebKitDOMHTMLOptGroupElement = object(TWebKitDOMHTMLElement)
    function get_disabled: gboolean; cdecl; inline;
    function get_label: Pgchar; cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_label(value: Pgchar); cdecl; inline;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property label_:  Pgchar read get_label  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLOptGroupElementClass = ^PWebKitDOMHTMLOptGroupElementClass;
  PWebKitDOMHTMLOptGroupElementClass = ^TWebKitDOMHTMLOptGroupElementClass;
  TWebKitDOMHTMLOptGroupElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLOptionElementClass = ^PWebKitDOMHTMLOptionElementClass;
  PWebKitDOMHTMLOptionElementClass = ^TWebKitDOMHTMLOptionElementClass;
  TWebKitDOMHTMLOptionElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLOptionsCollection = ^PWebKitDOMHTMLOptionsCollection;
  PWebKitDOMHTMLOptionsCollection = ^TWebKitDOMHTMLOptionsCollection;
  TWebKitDOMHTMLOptionsCollection = object(TWebKitDOMHTMLCollection)
    function get_selected_index: glong; cdecl; inline;
    procedure set_selected_index(value: glong); cdecl; inline;
    property selected_index:  glong read get_selected_index  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLOptionsCollectionClass = ^PWebKitDOMHTMLOptionsCollectionClass;
  PWebKitDOMHTMLOptionsCollectionClass = ^TWebKitDOMHTMLOptionsCollectionClass;
  TWebKitDOMHTMLOptionsCollectionClass = object
    parent_class: TWebKitDOMHTMLCollectionClass;
  end;

  PPWebKitDOMHTMLParagraphElement = ^PWebKitDOMHTMLParagraphElement;
  PWebKitDOMHTMLParagraphElement = ^TWebKitDOMHTMLParagraphElement;
  TWebKitDOMHTMLParagraphElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLParagraphElementClass = ^PWebKitDOMHTMLParagraphElementClass;
  PWebKitDOMHTMLParagraphElementClass = ^TWebKitDOMHTMLParagraphElementClass;
  TWebKitDOMHTMLParagraphElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLParamElement = ^PWebKitDOMHTMLParamElement;
  PWebKitDOMHTMLParamElement = ^TWebKitDOMHTMLParamElement;
  TWebKitDOMHTMLParamElement = object(TWebKitDOMHTMLElement)
    function get_name: Pgchar; cdecl; inline;
    function get_value: Pgchar; cdecl; inline;
    function get_value_type: Pgchar; cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_value(value: Pgchar); cdecl; inline;
    procedure set_value_type(value: Pgchar); cdecl; inline;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
    property value:  Pgchar read get_value  { property is writeable but setter not declared } ;
    property value_type:  Pgchar read get_value_type  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLParamElementClass = ^PWebKitDOMHTMLParamElementClass;
  PWebKitDOMHTMLParamElementClass = ^TWebKitDOMHTMLParamElementClass;
  TWebKitDOMHTMLParamElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLPreElement = ^PWebKitDOMHTMLPreElement;
  PWebKitDOMHTMLPreElement = ^TWebKitDOMHTMLPreElement;
  TWebKitDOMHTMLPreElement = object(TWebKitDOMHTMLElement)
    function get_width: glong; cdecl; inline;
    function get_wrap: gboolean; cdecl; inline;
    procedure set_width(value: glong); cdecl; inline;
    procedure set_wrap(value: gboolean); cdecl; inline;
    property width:  glong read get_width  { property is writeable but setter not declared } ;
    property wrap:  gboolean read get_wrap  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLPreElementClass = ^PWebKitDOMHTMLPreElementClass;
  PWebKitDOMHTMLPreElementClass = ^TWebKitDOMHTMLPreElementClass;
  TWebKitDOMHTMLPreElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLQuoteElement = ^PWebKitDOMHTMLQuoteElement;
  PWebKitDOMHTMLQuoteElement = ^TWebKitDOMHTMLQuoteElement;
  TWebKitDOMHTMLQuoteElement = object(TWebKitDOMHTMLElement)
    function get_cite: Pgchar; cdecl; inline;
    procedure set_cite(value: Pgchar); cdecl; inline;
    property cite:  Pgchar read get_cite  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLQuoteElementClass = ^PWebKitDOMHTMLQuoteElementClass;
  PWebKitDOMHTMLQuoteElementClass = ^TWebKitDOMHTMLQuoteElementClass;
  TWebKitDOMHTMLQuoteElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLScriptElement = ^PWebKitDOMHTMLScriptElement;
  PWebKitDOMHTMLScriptElement = ^TWebKitDOMHTMLScriptElement;
  TWebKitDOMHTMLScriptElement = object(TWebKitDOMHTMLElement)
    function get_async: gboolean; cdecl; inline;
    function get_charset: Pgchar; cdecl; inline;
    function get_defer: gboolean; cdecl; inline;
    function get_event: Pgchar; cdecl; inline;
    function get_html_for: Pgchar; cdecl; inline;
    function get_src: Pgchar; cdecl; inline;
    function get_text: Pgchar; cdecl; inline;
    procedure set_async(value: gboolean); cdecl; inline;
    procedure set_charset(value: Pgchar); cdecl; inline;
    procedure set_defer(value: gboolean); cdecl; inline;
    procedure set_event(value: Pgchar); cdecl; inline;
    procedure set_html_for(value: Pgchar); cdecl; inline;
    procedure set_src(value: Pgchar); cdecl; inline;
    procedure set_text(value: Pgchar); cdecl; inline;
    property async:  gboolean read get_async  { property is writeable but setter not declared } ;
    property charset:  Pgchar read get_charset  { property is writeable but setter not declared } ;
    property defer:  gboolean read get_defer  { property is writeable but setter not declared } ;
    property event:  Pgchar read get_event  { property is writeable but setter not declared } ;
    property html_for:  Pgchar read get_html_for  { property is writeable but setter not declared } ;
    property src:  Pgchar read get_src  { property is writeable but setter not declared } ;
    property text:  Pgchar read get_text  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLScriptElementClass = ^PWebKitDOMHTMLScriptElementClass;
  PWebKitDOMHTMLScriptElementClass = ^TWebKitDOMHTMLScriptElementClass;
  TWebKitDOMHTMLScriptElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLSelectElement = ^PWebKitDOMHTMLSelectElement;
  PWebKitDOMHTMLSelectElement = ^TWebKitDOMHTMLSelectElement;
  TWebKitDOMHTMLSelectElement = object(TWebKitDOMHTMLElement)
    procedure add(element: PWebKitDOMHTMLElement; before: PWebKitDOMHTMLElement); cdecl; inline;
    function check_validity: gboolean; cdecl; inline;
    function get_autofocus: gboolean; cdecl; inline;
    function get_disabled: gboolean; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_labels: PWebKitDOMNodeList; cdecl; inline;
    function get_length: gulong; cdecl; inline;
    function get_multiple: gboolean; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_options: PWebKitDOMHTMLOptionsCollection; cdecl; inline;
    function get_required: gboolean; cdecl; inline;
    function get_selected_index: glong; cdecl; inline;
    function get_size: glong; cdecl; inline;
    function get_validation_message: Pgchar; cdecl; inline;
    function get_validity: PWebKitDOMValidityState; cdecl; inline;
    function get_value: Pgchar; cdecl; inline;
    function get_will_validate: gboolean; cdecl; inline;
    function item(index: gulong): PWebKitDOMNode; cdecl; inline;
    function named_item(name: Pgchar): PWebKitDOMNode; cdecl; inline;
    procedure remove(index: glong); cdecl; inline;
    procedure set_autofocus(value: gboolean); cdecl; inline;
    procedure set_custom_validity(error: Pgchar); cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_length(value: gulong); cdecl; inline;
    procedure set_multiple(value: gboolean); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_required(value: gboolean); cdecl; inline;
    procedure set_selected_index(value: glong); cdecl; inline;
    procedure set_size(value: glong); cdecl; inline;
    procedure set_value(value: Pgchar); cdecl; inline;
    property autofocus:  gboolean read get_autofocus  { property is writeable but setter not declared } ;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property labels:  PWebKitDOMNodeList read get_labels ;
    property length:  gulong read get_length  { property is writeable but setter not declared } ;
    property multiple:  gboolean read get_multiple  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property options:  PWebKitDOMHTMLOptionsCollection read get_options ;
    property required:  gboolean read get_required  { property is writeable but setter not declared } ;
    property selected_index:  glong read get_selected_index  { property is writeable but setter not declared } ;
    property size:  glong read get_size  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
    property validation_message:  Pgchar read get_validation_message ;
    property validity:  PWebKitDOMValidityState read get_validity ;
    property value:  Pgchar read get_value  { property is writeable but setter not declared } ;
    property will_validate:  gboolean read get_will_validate ;
  end;

  PPWebKitDOMHTMLSelectElementClass = ^PWebKitDOMHTMLSelectElementClass;
  PWebKitDOMHTMLSelectElementClass = ^TWebKitDOMHTMLSelectElementClass;
  TWebKitDOMHTMLSelectElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLStyleElement = ^PWebKitDOMHTMLStyleElement;
  PWebKitDOMHTMLStyleElement = ^TWebKitDOMHTMLStyleElement;
  TWebKitDOMHTMLStyleElement = object(TWebKitDOMHTMLElement)
    function get_disabled: gboolean; cdecl; inline;
    function get_media: Pgchar; cdecl; inline;
    function get_sheet: PWebKitDOMStyleSheet; cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_media(value: Pgchar); cdecl; inline;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property media:  Pgchar read get_media  { property is writeable but setter not declared } ;
    property sheet:  PWebKitDOMStyleSheet read get_sheet ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLStyleElementClass = ^PWebKitDOMHTMLStyleElementClass;
  PWebKitDOMHTMLStyleElementClass = ^TWebKitDOMHTMLStyleElementClass;
  TWebKitDOMHTMLStyleElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLTableCaptionElement = ^PWebKitDOMHTMLTableCaptionElement;
  PWebKitDOMHTMLTableCaptionElement = ^TWebKitDOMHTMLTableCaptionElement;
  TWebKitDOMHTMLTableCaptionElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLTableCaptionElementClass = ^PWebKitDOMHTMLTableCaptionElementClass;
  PWebKitDOMHTMLTableCaptionElementClass = ^TWebKitDOMHTMLTableCaptionElementClass;
  TWebKitDOMHTMLTableCaptionElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLTableCellElement = ^PWebKitDOMHTMLTableCellElement;
  PWebKitDOMHTMLTableCellElement = ^TWebKitDOMHTMLTableCellElement;
  TWebKitDOMHTMLTableCellElement = object(TWebKitDOMHTMLElement)
    function get_abbr: Pgchar; cdecl; inline;
    function get_align: Pgchar; cdecl; inline;
    function get_axis: Pgchar; cdecl; inline;
    function get_bg_color: Pgchar; cdecl; inline;
    function get_cell_index: glong; cdecl; inline;
    function get_ch: Pgchar; cdecl; inline;
    function get_ch_off: Pgchar; cdecl; inline;
    function get_col_span: glong; cdecl; inline;
    function get_headers: Pgchar; cdecl; inline;
    function get_height: Pgchar; cdecl; inline;
    function get_no_wrap: gboolean; cdecl; inline;
    function get_row_span: glong; cdecl; inline;
    function get_scope: Pgchar; cdecl; inline;
    function get_v_align: Pgchar; cdecl; inline;
    function get_width: Pgchar; cdecl; inline;
    procedure set_abbr(value: Pgchar); cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_axis(value: Pgchar); cdecl; inline;
    procedure set_bg_color(value: Pgchar); cdecl; inline;
    procedure set_ch(value: Pgchar); cdecl; inline;
    procedure set_ch_off(value: Pgchar); cdecl; inline;
    procedure set_col_span(value: glong); cdecl; inline;
    procedure set_headers(value: Pgchar); cdecl; inline;
    procedure set_height(value: Pgchar); cdecl; inline;
    procedure set_no_wrap(value: gboolean); cdecl; inline;
    procedure set_row_span(value: glong); cdecl; inline;
    procedure set_scope(value: Pgchar); cdecl; inline;
    procedure set_v_align(value: Pgchar); cdecl; inline;
    procedure set_width(value: Pgchar); cdecl; inline;
    property abbr:  Pgchar read get_abbr  { property is writeable but setter not declared } ;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property axis:  Pgchar read get_axis  { property is writeable but setter not declared } ;
    property bg_color:  Pgchar read get_bg_color  { property is writeable but setter not declared } ;
    property cell_index:  glong read get_cell_index ;
    property ch:  Pgchar read get_ch  { property is writeable but setter not declared } ;
    property ch_off:  Pgchar read get_ch_off  { property is writeable but setter not declared } ;
    property col_span:  glong read get_col_span  { property is writeable but setter not declared } ;
    property headers:  Pgchar read get_headers  { property is writeable but setter not declared } ;
    property height:  Pgchar read get_height  { property is writeable but setter not declared } ;
    property no_wrap:  gboolean read get_no_wrap  { property is writeable but setter not declared } ;
    property row_span:  glong read get_row_span  { property is writeable but setter not declared } ;
    property scope:  Pgchar read get_scope  { property is writeable but setter not declared } ;
    property v_align:  Pgchar read get_v_align  { property is writeable but setter not declared } ;
    property width:  Pgchar read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLTableCellElementClass = ^PWebKitDOMHTMLTableCellElementClass;
  PWebKitDOMHTMLTableCellElementClass = ^TWebKitDOMHTMLTableCellElementClass;
  TWebKitDOMHTMLTableCellElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLTableColElement = ^PWebKitDOMHTMLTableColElement;
  PWebKitDOMHTMLTableColElement = ^TWebKitDOMHTMLTableColElement;
  TWebKitDOMHTMLTableColElement = object(TWebKitDOMHTMLElement)
    function get_align: Pgchar; cdecl; inline;
    function get_ch: Pgchar; cdecl; inline;
    function get_ch_off: Pgchar; cdecl; inline;
    function get_span: glong; cdecl; inline;
    function get_v_align: Pgchar; cdecl; inline;
    function get_width: Pgchar; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_ch(value: Pgchar); cdecl; inline;
    procedure set_ch_off(value: Pgchar); cdecl; inline;
    procedure set_span(value: glong); cdecl; inline;
    procedure set_v_align(value: Pgchar); cdecl; inline;
    procedure set_width(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property ch:  Pgchar read get_ch  { property is writeable but setter not declared } ;
    property ch_off:  Pgchar read get_ch_off  { property is writeable but setter not declared } ;
    property span:  glong read get_span  { property is writeable but setter not declared } ;
    property v_align:  Pgchar read get_v_align  { property is writeable but setter not declared } ;
    property width:  Pgchar read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLTableColElementClass = ^PWebKitDOMHTMLTableColElementClass;
  PWebKitDOMHTMLTableColElementClass = ^TWebKitDOMHTMLTableColElementClass;
  TWebKitDOMHTMLTableColElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLTableSectionElement = ^PWebKitDOMHTMLTableSectionElement;
  PWebKitDOMHTMLTableSectionElement = ^TWebKitDOMHTMLTableSectionElement;
  TWebKitDOMHTMLTableSectionElement = object(TWebKitDOMHTMLElement)
    procedure delete_row(index: glong); cdecl; inline;
    function get_align: Pgchar; cdecl; inline;
    function get_ch: Pgchar; cdecl; inline;
    function get_ch_off: Pgchar; cdecl; inline;
    function get_rows: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_v_align: Pgchar; cdecl; inline;
    function insert_row(index: glong): PWebKitDOMHTMLElement; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_ch(value: Pgchar); cdecl; inline;
    procedure set_ch_off(value: Pgchar); cdecl; inline;
    procedure set_v_align(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property ch:  Pgchar read get_ch  { property is writeable but setter not declared } ;
    property ch_off:  Pgchar read get_ch_off  { property is writeable but setter not declared } ;
    property rows:  PWebKitDOMHTMLCollection read get_rows ;
    property v_align:  Pgchar read get_v_align  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLTableElement = ^PWebKitDOMHTMLTableElement;
  PWebKitDOMHTMLTableElement = ^TWebKitDOMHTMLTableElement;
  TWebKitDOMHTMLTableElement = object(TWebKitDOMHTMLElement)
    function create_caption: PWebKitDOMHTMLElement; cdecl; inline;
    function create_t_foot: PWebKitDOMHTMLElement; cdecl; inline;
    function create_t_head: PWebKitDOMHTMLElement; cdecl; inline;
    procedure delete_caption; cdecl; inline;
    procedure delete_row(index: glong); cdecl; inline;
    procedure delete_t_foot; cdecl; inline;
    procedure delete_t_head; cdecl; inline;
    function get_align: Pgchar; cdecl; inline;
    function get_bg_color: Pgchar; cdecl; inline;
    function get_border: Pgchar; cdecl; inline;
    function get_caption: PWebKitDOMHTMLTableCaptionElement; cdecl; inline;
    function get_cell_padding: Pgchar; cdecl; inline;
    function get_cell_spacing: Pgchar; cdecl; inline;
    function get_frame: Pgchar; cdecl; inline;
    function get_rows: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_rules: Pgchar; cdecl; inline;
    function get_summary: Pgchar; cdecl; inline;
    function get_t_bodies: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_t_foot: PWebKitDOMHTMLTableSectionElement; cdecl; inline;
    function get_t_head: PWebKitDOMHTMLTableSectionElement; cdecl; inline;
    function get_width: Pgchar; cdecl; inline;
    function insert_row(index: glong): PWebKitDOMHTMLElement; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_bg_color(value: Pgchar); cdecl; inline;
    procedure set_border(value: Pgchar); cdecl; inline;
    procedure set_caption(value: PWebKitDOMHTMLTableCaptionElement); cdecl; inline;
    procedure set_cell_padding(value: Pgchar); cdecl; inline;
    procedure set_cell_spacing(value: Pgchar); cdecl; inline;
    procedure set_frame(value: Pgchar); cdecl; inline;
    procedure set_rules(value: Pgchar); cdecl; inline;
    procedure set_summary(value: Pgchar); cdecl; inline;
    procedure set_t_foot(value: PWebKitDOMHTMLTableSectionElement); cdecl; inline;
    procedure set_t_head(value: PWebKitDOMHTMLTableSectionElement); cdecl; inline;
    procedure set_width(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property bg_color:  Pgchar read get_bg_color  { property is writeable but setter not declared } ;
    property border:  Pgchar read get_border  { property is writeable but setter not declared } ;
    property caption:  PWebKitDOMHTMLTableCaptionElement read get_caption  { property is writeable but setter not declared } ;
    property cell_padding:  Pgchar read get_cell_padding  { property is writeable but setter not declared } ;
    property cell_spacing:  Pgchar read get_cell_spacing  { property is writeable but setter not declared } ;
    property frame:  Pgchar read get_frame  { property is writeable but setter not declared } ;
    property rows:  PWebKitDOMHTMLCollection read get_rows ;
    property rules:  Pgchar read get_rules  { property is writeable but setter not declared } ;
    property summary:  Pgchar read get_summary  { property is writeable but setter not declared } ;
    property t_bodies:  PWebKitDOMHTMLCollection read get_t_bodies ;
    property t_foot:  PWebKitDOMHTMLTableSectionElement read get_t_foot  { property is writeable but setter not declared } ;
    property t_head:  PWebKitDOMHTMLTableSectionElement read get_t_head  { property is writeable but setter not declared } ;
    property width:  Pgchar read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLTableElementClass = ^PWebKitDOMHTMLTableElementClass;
  PWebKitDOMHTMLTableElementClass = ^TWebKitDOMHTMLTableElementClass;
  TWebKitDOMHTMLTableElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLTableRowElement = ^PWebKitDOMHTMLTableRowElement;
  PWebKitDOMHTMLTableRowElement = ^TWebKitDOMHTMLTableRowElement;
  TWebKitDOMHTMLTableRowElement = object(TWebKitDOMHTMLElement)
    procedure delete_cell(index: glong); cdecl; inline;
    function get_align: Pgchar; cdecl; inline;
    function get_bg_color: Pgchar; cdecl; inline;
    function get_cells: PWebKitDOMHTMLCollection; cdecl; inline;
    function get_ch: Pgchar; cdecl; inline;
    function get_ch_off: Pgchar; cdecl; inline;
    function get_row_index: glong; cdecl; inline;
    function get_section_row_index: glong; cdecl; inline;
    function get_v_align: Pgchar; cdecl; inline;
    function insert_cell(index: glong): PWebKitDOMHTMLElement; cdecl; inline;
    procedure set_align(value: Pgchar); cdecl; inline;
    procedure set_bg_color(value: Pgchar); cdecl; inline;
    procedure set_ch(value: Pgchar); cdecl; inline;
    procedure set_ch_off(value: Pgchar); cdecl; inline;
    procedure set_v_align(value: Pgchar); cdecl; inline;
    property align:  Pgchar read get_align  { property is writeable but setter not declared } ;
    property bg_color:  Pgchar read get_bg_color  { property is writeable but setter not declared } ;
    property cells:  PWebKitDOMHTMLCollection read get_cells ;
    property ch:  Pgchar read get_ch  { property is writeable but setter not declared } ;
    property ch_off:  Pgchar read get_ch_off  { property is writeable but setter not declared } ;
    property row_index:  glong read get_row_index ;
    property section_row_index:  glong read get_section_row_index ;
    property v_align:  Pgchar read get_v_align  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLTableRowElementClass = ^PWebKitDOMHTMLTableRowElementClass;
  PWebKitDOMHTMLTableRowElementClass = ^TWebKitDOMHTMLTableRowElementClass;
  TWebKitDOMHTMLTableRowElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLTableSectionElementClass = ^PWebKitDOMHTMLTableSectionElementClass;
  PWebKitDOMHTMLTableSectionElementClass = ^TWebKitDOMHTMLTableSectionElementClass;
  TWebKitDOMHTMLTableSectionElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLTextAreaElement = ^PWebKitDOMHTMLTextAreaElement;
  PWebKitDOMHTMLTextAreaElement = ^TWebKitDOMHTMLTextAreaElement;
  TWebKitDOMHTMLTextAreaElement = object(TWebKitDOMHTMLElement)
    function check_validity: gboolean; cdecl; inline;
    function get_access_key: Pgchar; cdecl; inline;
    function get_autofocus: gboolean; cdecl; inline;
    function get_cols: glong; cdecl; inline;
    function get_default_value: Pgchar; cdecl; inline;
    function get_disabled: gboolean; cdecl; inline;
    function get_form: PWebKitDOMHTMLFormElement; cdecl; inline;
    function get_labels: PWebKitDOMNodeList; cdecl; inline;
    function get_max_length: glong; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_placeholder: Pgchar; cdecl; inline;
    function get_read_only: gboolean; cdecl; inline;
    function get_required: gboolean; cdecl; inline;
    function get_rows: glong; cdecl; inline;
    function get_selection_end: glong; cdecl; inline;
    function get_selection_start: glong; cdecl; inline;
    function get_text_length: gulong; cdecl; inline;
    function get_validation_message: Pgchar; cdecl; inline;
    function get_validity: PWebKitDOMValidityState; cdecl; inline;
    function get_value: Pgchar; cdecl; inline;
    function get_will_validate: gboolean; cdecl; inline;
    procedure select; cdecl; inline;
    procedure set_access_key(value: Pgchar); cdecl; inline;
    procedure set_autofocus(value: gboolean); cdecl; inline;
    procedure set_cols(value: glong); cdecl; inline;
    procedure set_custom_validity(error: Pgchar); cdecl; inline;
    procedure set_default_value(value: Pgchar); cdecl; inline;
    procedure set_disabled(value: gboolean); cdecl; inline;
    procedure set_max_length(value: glong); cdecl; inline;
    procedure set_name(value: Pgchar); cdecl; inline;
    procedure set_placeholder(value: Pgchar); cdecl; inline;
    procedure set_read_only(value: gboolean); cdecl; inline;
    procedure set_required(value: gboolean); cdecl; inline;
    procedure set_rows(value: glong); cdecl; inline;
    procedure set_selection_end(value: glong); cdecl; inline;
    procedure set_selection_range(start: glong; end_: glong); cdecl; inline;
    procedure set_selection_start(value: glong); cdecl; inline;
    procedure set_value(value: Pgchar); cdecl; inline;
    property access_key:  Pgchar read get_access_key  { property is writeable but setter not declared } ;
    property autofocus:  gboolean read get_autofocus  { property is writeable but setter not declared } ;
    property cols:  glong read get_cols  { property is writeable but setter not declared } ;
    property default_value:  Pgchar read get_default_value  { property is writeable but setter not declared } ;
    property disabled:  gboolean read get_disabled  { property is writeable but setter not declared } ;
    property form:  PWebKitDOMHTMLFormElement read get_form ;
    property labels:  PWebKitDOMNodeList read get_labels ;
    property max_length:  glong read get_max_length  { property is writeable but setter not declared } ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property placeholder:  Pgchar read get_placeholder  { property is writeable but setter not declared } ;
    property read_only:  gboolean read get_read_only  { property is writeable but setter not declared } ;
    property required:  gboolean read get_required  { property is writeable but setter not declared } ;
    property rows:  glong read get_rows  { property is writeable but setter not declared } ;
    property selection_end:  glong read get_selection_end  { property is writeable but setter not declared } ;
    property selection_start:  glong read get_selection_start  { property is writeable but setter not declared } ;
    property text_length:  gulong read get_text_length ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type ;
    property validation_message:  Pgchar read get_validation_message ;
    property validity:  PWebKitDOMValidityState read get_validity ;
    property value:  Pgchar read get_value  { property is writeable but setter not declared } ;
    property will_validate:  gboolean read get_will_validate ;
  end;

  PPWebKitDOMHTMLTextAreaElementClass = ^PWebKitDOMHTMLTextAreaElementClass;
  PWebKitDOMHTMLTextAreaElementClass = ^TWebKitDOMHTMLTextAreaElementClass;
  TWebKitDOMHTMLTextAreaElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLTitleElement = ^PWebKitDOMHTMLTitleElement;
  PWebKitDOMHTMLTitleElement = ^TWebKitDOMHTMLTitleElement;
  TWebKitDOMHTMLTitleElement = object(TWebKitDOMHTMLElement)
    function get_text: Pgchar; cdecl; inline;
    procedure set_text(value: Pgchar); cdecl; inline;
    property text:  Pgchar read get_text  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLTitleElementClass = ^PWebKitDOMHTMLTitleElementClass;
  PWebKitDOMHTMLTitleElementClass = ^TWebKitDOMHTMLTitleElementClass;
  TWebKitDOMHTMLTitleElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLUListElement = ^PWebKitDOMHTMLUListElement;
  PWebKitDOMHTMLUListElement = ^TWebKitDOMHTMLUListElement;
  TWebKitDOMHTMLUListElement = object(TWebKitDOMHTMLElement)
    function get_compact: gboolean; cdecl; inline;
    procedure set_compact(value: gboolean); cdecl; inline;
    property compact:  gboolean read get_compact  { property is writeable but setter not declared } ;
    //property type_: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_type  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLUListElementClass = ^PWebKitDOMHTMLUListElementClass;
  PWebKitDOMHTMLUListElementClass = ^TWebKitDOMHTMLUListElementClass;
  TWebKitDOMHTMLUListElementClass = object
    parent_class: TWebKitDOMHTMLElementClass;
  end;

  PPWebKitDOMHTMLVideoElement = ^PWebKitDOMHTMLVideoElement;
  PWebKitDOMHTMLVideoElement = ^TWebKitDOMHTMLVideoElement;
  TWebKitDOMHTMLVideoElement = object(TWebKitDOMHTMLMediaElement)
    function get_height: gulong; cdecl; inline;
    function get_poster: Pgchar; cdecl; inline;
    function get_video_height: gulong; cdecl; inline;
    function get_video_width: gulong; cdecl; inline;
    function get_webkit_displaying_fullscreen: gboolean; cdecl; inline;
    function get_webkit_supports_fullscreen: gboolean; cdecl; inline;
    function get_width: gulong; cdecl; inline;
    procedure set_height(value: gulong); cdecl; inline;
    procedure set_poster(value: Pgchar); cdecl; inline;
    procedure set_width(value: gulong); cdecl; inline;
    procedure webkit_enter_full_screen(isUserGesture: gboolean); cdecl; inline;
    procedure webkit_enter_fullscreen(isUserGesture: gboolean); cdecl; inline;
    procedure webkit_exit_full_screen; cdecl; inline;
    procedure webkit_exit_fullscreen; cdecl; inline;
    property height:  gulong read get_height  { property is writeable but setter not declared } ;
    property poster:  Pgchar read get_poster  { property is writeable but setter not declared } ;
    property video_height:  gulong read get_video_height ;
    property video_width:  gulong read get_video_width ;
    property webkit_displaying_fullscreen:  gboolean read get_webkit_displaying_fullscreen ;
    property webkit_supports_fullscreen:  gboolean read get_webkit_supports_fullscreen ;
    property width:  gulong read get_width  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMHTMLVideoElementClass = ^PWebKitDOMHTMLVideoElementClass;
  PWebKitDOMHTMLVideoElementClass = ^TWebKitDOMHTMLVideoElementClass;
  TWebKitDOMHTMLVideoElementClass = object
    parent_class: TWebKitDOMHTMLMediaElementClass;
  end;

  PPWebKitDOMHistoryClass = ^PWebKitDOMHistoryClass;
  PWebKitDOMHistoryClass = ^TWebKitDOMHistoryClass;
  TWebKitDOMHistoryClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMLocation = ^PWebKitDOMLocation;
  PWebKitDOMLocation = ^TWebKitDOMLocation;
  TWebKitDOMLocation = object(TWebKitDOMObject)
    function get_origin: Pgchar; cdecl; inline;
    function get_parameter(name: Pgchar): Pgchar; cdecl; inline;
    property origin:  Pgchar read get_origin ;
  end;

  PPWebKitDOMLocationClass = ^PWebKitDOMLocationClass;
  PWebKitDOMLocationClass = ^TWebKitDOMLocationClass;
  TWebKitDOMLocationClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMMediaErrorClass = ^PWebKitDOMMediaErrorClass;
  PWebKitDOMMediaErrorClass = ^TWebKitDOMMediaErrorClass;
  TWebKitDOMMediaErrorClass = object
    parent_class: TWebKitDOMObjectClass;
  end;
  TWebKitDOMMediaList = object(TWebKitDOMObject)
    procedure append_medium(new_medium: Pgchar); cdecl; inline;
    procedure delete_medium(old_medium: Pgchar); cdecl; inline;
    function get_length: gulong; cdecl; inline;
    function get_media_text: Pgchar; cdecl; inline;
    function item(index: gulong): Pgchar; cdecl; inline;
    procedure set_media_text(value: Pgchar); cdecl; inline;
    property length:  gulong read get_length ;
    property media_text:  Pgchar read get_media_text  { property is writeable but setter not declared } ;
  end;

  PPWebKitDOMMediaListClass = ^PWebKitDOMMediaListClass;
  PWebKitDOMMediaListClass = ^TWebKitDOMMediaListClass;
  TWebKitDOMMediaListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMMediaQueryListClass = ^PWebKitDOMMediaQueryListClass;
  PWebKitDOMMediaQueryListClass = ^TWebKitDOMMediaQueryListClass;
  TWebKitDOMMediaQueryListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMMemoryInfoClass = ^PWebKitDOMMemoryInfoClass;
  PWebKitDOMMemoryInfoClass = ^TWebKitDOMMemoryInfoClass;
  TWebKitDOMMemoryInfoClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMMessagePort = ^PWebKitDOMMessagePort;
  PWebKitDOMMessagePort = ^TWebKitDOMMessagePort;
  TWebKitDOMMessagePort = object(TWebKitDOMObject)
  end;

  PPWebKitDOMMessagePortClass = ^PWebKitDOMMessagePortClass;
  PWebKitDOMMessagePortClass = ^TWebKitDOMMessagePortClass;
  TWebKitDOMMessagePortClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMUIEvent = ^PWebKitDOMUIEvent;
  PWebKitDOMUIEvent = ^TWebKitDOMUIEvent;
  TWebKitDOMUIEvent = object(TWebKitDOMEvent)
    function get_char_code: glong; cdecl; inline;
    function get_detail: glong; cdecl; inline;
    function get_key_code: glong; cdecl; inline;
    function get_layer_x: glong; cdecl; inline;
    function get_layer_y: glong; cdecl; inline;
    function get_page_x: glong; cdecl; inline;
    function get_page_y: glong; cdecl; inline;
    function get_view: PWebKitDOMDOMWindow; cdecl; inline;
    function get_which: glong; cdecl; inline;
    procedure init_ui_event(type_: Pgchar; can_bubble: gboolean; cancelable: gboolean; view: PWebKitDOMDOMWindow; detail: glong); cdecl; inline;
    property char_code:  glong read get_char_code ;
    property detail:  glong read get_detail ;
    property key_code:  glong read get_key_code ;
    property layer_x:  glong read get_layer_x ;
    property layer_y:  glong read get_layer_y ;
    property page_x:  glong read get_page_x ;
    property page_y:  glong read get_page_y ;
    property view:  PWebKitDOMDOMWindow read get_view ;
    property which:  glong read get_which ;
  end;

  PPWebKitDOMMouseEvent = ^PWebKitDOMMouseEvent;
  PWebKitDOMMouseEvent = ^TWebKitDOMMouseEvent;
  TWebKitDOMMouseEvent = object(TWebKitDOMUIEvent)
    function get_alt_key: gboolean; cdecl; inline;
    function get_button: gushort; cdecl; inline;
    function get_client_x: glong; cdecl; inline;
    function get_client_y: glong; cdecl; inline;
    function get_ctrl_key: gboolean; cdecl; inline;
    function get_from_element: PWebKitDOMNode; cdecl; inline;
    function get_meta_key: gboolean; cdecl; inline;
    function get_offset_x: glong; cdecl; inline;
    function get_offset_y: glong; cdecl; inline;
    function get_related_target: PWebKitDOMEventTarget; cdecl; inline;
    function get_screen_x: glong; cdecl; inline;
    function get_screen_y: glong; cdecl; inline;
    function get_shift_key: gboolean; cdecl; inline;
    function get_to_element: PWebKitDOMNode; cdecl; inline;
    function get_x: glong; cdecl; inline;
    function get_y: glong; cdecl; inline;
    procedure init_mouse_event(type_: Pgchar; can_bubble: gboolean; cancelable: gboolean; view: PWebKitDOMDOMWindow; detail: glong; screen_x: glong; screen_y: glong; client_x: glong; client_y: glong; ctrl_key: gboolean; alt_key: gboolean; shift_key: gboolean; meta_key: gboolean; button: gushort; related_target: PWebKitDOMEventTarget); cdecl; inline;
    property alt_key:  gboolean read get_alt_key ;
    property button:  gushort read get_button ;
    property client_x:  glong read get_client_x ;
    property client_y:  glong read get_client_y ;
    property ctrl_key:  gboolean read get_ctrl_key ;
    property from_element:  PWebKitDOMNode read get_from_element ;
    property meta_key:  gboolean read get_meta_key ;
    property offset_x:  glong read get_offset_x ;
    property offset_y:  glong read get_offset_y ;
    property related_target:  PWebKitDOMEventTarget read get_related_target ;
    property screen_x:  glong read get_screen_x ;
    property screen_y:  glong read get_screen_y ;
    property shift_key:  gboolean read get_shift_key ;
    property to_element:  PWebKitDOMNode read get_to_element ;
    property x:  glong read get_x ;
    property y:  glong read get_y ;
  end;

  PPWebKitDOMUIEventClass = ^PWebKitDOMUIEventClass;
  PWebKitDOMUIEventClass = ^TWebKitDOMUIEventClass;
  TWebKitDOMUIEventClass = object
    parent_class: TWebKitDOMEventClass;
  end;

  PPWebKitDOMMouseEventClass = ^PWebKitDOMMouseEventClass;
  PWebKitDOMMouseEventClass = ^TWebKitDOMMouseEventClass;
  TWebKitDOMMouseEventClass = object
    parent_class: TWebKitDOMUIEventClass;
  end;

  PPWebKitDOMNamedNodeMapClass = ^PWebKitDOMNamedNodeMapClass;
  PWebKitDOMNamedNodeMapClass = ^TWebKitDOMNamedNodeMapClass;
  TWebKitDOMNamedNodeMapClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMNavigator = ^PWebKitDOMNavigator;
  PWebKitDOMNavigator = ^TWebKitDOMNavigator;
  TWebKitDOMNavigator = object(TWebKitDOMObject)
    function get_app_code_name: Pgchar; cdecl; inline;
    function get_app_name: Pgchar; cdecl; inline;
    function get_app_version: Pgchar; cdecl; inline;
    function get_cookie_enabled: gboolean; cdecl; inline;
    function get_language: Pgchar; cdecl; inline;
    function get_mime_types: PWebKitDOMDOMMimeTypeArray; cdecl; inline;
    function get_on_line: gboolean; cdecl; inline;
    function get_platform: Pgchar; cdecl; inline;
    function get_plugins: PWebKitDOMDOMPluginArray; cdecl; inline;
    function get_product: Pgchar; cdecl; inline;
    function get_product_sub: Pgchar; cdecl; inline;
    procedure get_storage_updates; cdecl; inline;
    function get_user_agent: Pgchar; cdecl; inline;
    function get_vendor: Pgchar; cdecl; inline;
    function get_vendor_sub: Pgchar; cdecl; inline;
    function java_enabled: gboolean; cdecl; inline;
    property app_code_name:  Pgchar read get_app_code_name ;
    property app_name:  Pgchar read get_app_name ;
    property app_version:  Pgchar read get_app_version ;
    property cookie_enabled:  gboolean read get_cookie_enabled ;
    property language:  Pgchar read get_language ;
    property mime_types:  PWebKitDOMDOMMimeTypeArray read get_mime_types ;
    property on_line:  gboolean read get_on_line ;
    property platform:  Pgchar read get_platform ;
    property plugins:  PWebKitDOMDOMPluginArray read get_plugins ;
    property product:  Pgchar read get_product ;
    property product_sub:  Pgchar read get_product_sub ;
    property user_agent:  Pgchar read get_user_agent ;
    property vendor:  Pgchar read get_vendor ;
    property vendor_sub:  Pgchar read get_vendor_sub ;
  end;

  PPWebKitDOMNavigatorClass = ^PWebKitDOMNavigatorClass;
  PWebKitDOMNavigatorClass = ^TWebKitDOMNavigatorClass;
  TWebKitDOMNavigatorClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMNodeFilterClass = ^PWebKitDOMNodeFilterClass;
  PWebKitDOMNodeFilterClass = ^TWebKitDOMNodeFilterClass;
  TWebKitDOMNodeFilterClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMNodeIteratorClass = ^PWebKitDOMNodeIteratorClass;
  PWebKitDOMNodeIteratorClass = ^TWebKitDOMNodeIteratorClass;
  TWebKitDOMNodeIteratorClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMNodeListClass = ^PWebKitDOMNodeListClass;
  PWebKitDOMNodeListClass = ^TWebKitDOMNodeListClass;
  TWebKitDOMNodeListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMObjectPrivate = ^PWebKitDOMObjectPrivate;
  PWebKitDOMObjectPrivate = ^TWebKitDOMObjectPrivate;

  TWebKitDOMObjectPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitDOMProcessingInstructionClass = ^PWebKitDOMProcessingInstructionClass;
  PWebKitDOMProcessingInstructionClass = ^TWebKitDOMProcessingInstructionClass;
  TWebKitDOMProcessingInstructionClass = object
    parent_class: TWebKitDOMNodeClass;
  end;

  PPWebKitDOMRangeClass = ^PWebKitDOMRangeClass;
  PWebKitDOMRangeClass = ^TWebKitDOMRangeClass;
  TWebKitDOMRangeClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMScreen = ^PWebKitDOMScreen;
  PWebKitDOMScreen = ^TWebKitDOMScreen;
  TWebKitDOMScreen = object(TWebKitDOMObject)
    function get_avail_height: gulong; cdecl; inline;
    function get_avail_left: glong; cdecl; inline;
    function get_avail_top: glong; cdecl; inline;
    function get_avail_width: gulong; cdecl; inline;
    function get_color_depth: gulong; cdecl; inline;
    function get_height: gulong; cdecl; inline;
    function get_pixel_depth: gulong; cdecl; inline;
    function get_width: gulong; cdecl; inline;
    property avail_height:  gulong read get_avail_height ;
    property avail_left:  glong read get_avail_left ;
    property avail_top:  glong read get_avail_top ;
    property avail_width:  gulong read get_avail_width ;
    property color_depth:  gulong read get_color_depth ;
    property height:  gulong read get_height ;
    property pixel_depth:  gulong read get_pixel_depth ;
    property width:  gulong read get_width ;
  end;

  PPWebKitDOMScreenClass = ^PWebKitDOMScreenClass;
  PWebKitDOMScreenClass = ^TWebKitDOMScreenClass;
  TWebKitDOMScreenClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMStorageClass = ^PWebKitDOMStorageClass;
  PWebKitDOMStorageClass = ^TWebKitDOMStorageClass;
  TWebKitDOMStorageClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMStyleMediaClass = ^PWebKitDOMStyleMediaClass;
  PWebKitDOMStyleMediaClass = ^TWebKitDOMStyleMediaClass;
  TWebKitDOMStyleMediaClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMStyleSheetListClass = ^PWebKitDOMStyleSheetListClass;
  PWebKitDOMStyleSheetListClass = ^TWebKitDOMStyleSheetListClass;
  TWebKitDOMStyleSheetListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMTimeRangesClass = ^PWebKitDOMTimeRangesClass;
  PWebKitDOMTimeRangesClass = ^TWebKitDOMTimeRangesClass;
  TWebKitDOMTimeRangesClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMTreeWalkerClass = ^PWebKitDOMTreeWalkerClass;
  PWebKitDOMTreeWalkerClass = ^TWebKitDOMTreeWalkerClass;
  TWebKitDOMTreeWalkerClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMValidityStateClass = ^PWebKitDOMValidityStateClass;
  PWebKitDOMValidityStateClass = ^TWebKitDOMValidityStateClass;
  TWebKitDOMValidityStateClass = object
    parent_class: TWebKitDOMObjectClass;
  end;
  TWebKitDOMWebKitAnimation = object(TWebKitDOMObject)
    function get_delay: gdouble; cdecl; inline;
    function get_direction: gushort; cdecl; inline;
    function get_duration: gdouble; cdecl; inline;
    function get_elapsed_time: gdouble; cdecl; inline;
    function get_ended: gboolean; cdecl; inline;
    function get_fill_mode: gushort; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_paused: gboolean; cdecl; inline;
    procedure pause; cdecl; inline;
    procedure play; cdecl; inline;
    procedure set_elapsed_time(value: gdouble); cdecl; inline;
    property delay:  gdouble read get_delay ;
    property direction:  gushort read get_direction ;
    property duration:  gdouble read get_duration ;
    property elapsed_time:  gdouble read get_elapsed_time  { property is writeable but setter not declared } ;
    property ended:  gboolean read get_ended ;
    property fill_mode:  gushort read get_fill_mode ;
    //property iteration_count: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_iteration_count ;
    property name:  Pgchar read get_name ;
    property paused:  gboolean read get_paused ;
  end;

  PPWebKitDOMWebKitAnimationClass = ^PWebKitDOMWebKitAnimationClass;
  PWebKitDOMWebKitAnimationClass = ^TWebKitDOMWebKitAnimationClass;
  TWebKitDOMWebKitAnimationClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMWebKitAnimationListClass = ^PWebKitDOMWebKitAnimationListClass;
  PWebKitDOMWebKitAnimationListClass = ^TWebKitDOMWebKitAnimationListClass;
  TWebKitDOMWebKitAnimationListClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMWebKitPointClass = ^PWebKitDOMWebKitPointClass;
  PWebKitDOMWebKitPointClass = ^TWebKitDOMWebKitPointClass;
  TWebKitDOMWebKitPointClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMXPathExpressionClass = ^PWebKitDOMXPathExpressionClass;
  PWebKitDOMXPathExpressionClass = ^TWebKitDOMXPathExpressionClass;
  TWebKitDOMXPathExpressionClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMXPathNSResolverClass = ^PWebKitDOMXPathNSResolverClass;
  PWebKitDOMXPathNSResolverClass = ^TWebKitDOMXPathNSResolverClass;
  TWebKitDOMXPathNSResolverClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDOMXPathResultClass = ^PWebKitDOMXPathResultClass;
  PWebKitDOMXPathResultClass = ^TWebKitDOMXPathResultClass;
  TWebKitDOMXPathResultClass = object
    parent_class: TWebKitDOMObjectClass;
  end;

  PPWebKitDownload = ^PWebKitDownload;
  PWebKitDownload = ^TWebKitDownload;

  PPWebKitNetworkRequest = ^PWebKitNetworkRequest;
  PWebKitNetworkRequest = ^TWebKitNetworkRequest;

  PPWebKitNetworkResponse = ^PWebKitNetworkResponse;
  PWebKitNetworkResponse = ^TWebKitNetworkResponse;

  PPWebKitDownloadStatus = ^PWebKitDownloadStatus;
  PWebKitDownloadStatus = ^TWebKitDownloadStatus;

  PPWebKitDownloadPrivate = ^PWebKitDownloadPrivate;
  PWebKitDownloadPrivate = ^TWebKitDownloadPrivate;
  TWebKitDownload = object(TGObject)
    priv: PWebKitDownloadPrivate;
    function new(request: PWebKitNetworkRequest): PWebKitDownload; cdecl; inline; static;
    procedure cancel; cdecl; inline;
    function get_current_size: guint64; cdecl; inline;
    function get_destination_uri: Pgchar; cdecl; inline;
    function get_elapsed_time: gdouble; cdecl; inline;
    function get_network_request: PWebKitNetworkRequest; cdecl; inline;
    function get_network_response: PWebKitNetworkResponse; cdecl; inline;
    function get_progress: gdouble; cdecl; inline;
    function get_status: TWebKitDownloadStatus; cdecl; inline;
    function get_suggested_filename: Pgchar; cdecl; inline;
    function get_total_size: guint64; cdecl; inline;
    function get_uri: Pgchar; cdecl; inline;
    procedure set_destination_uri(destination_uri: Pgchar); cdecl; inline;
    procedure start; cdecl; inline;
    property current_size:  guint64 read get_current_size ;
    property destination_uri:  Pgchar read get_destination_uri  { property is writeable but setter not declared } ;
    property network_request:  PWebKitNetworkRequest read get_network_request  { property is writeable but setter not declared } ;
    property network_response:  PWebKitNetworkResponse read get_network_response  { property is writeable but setter not declared } ;
    property progress:  gdouble read get_progress ;
    property status:  TWebKitDownloadStatus read get_status ;
    property suggested_filename:  Pgchar read get_suggested_filename ;
    property total_size:  guint64 read get_total_size ;
  end;

  PPWebKitNetworkRequestPrivate = ^PWebKitNetworkRequestPrivate;
  PWebKitNetworkRequestPrivate = ^TWebKitNetworkRequestPrivate;
  TWebKitNetworkRequest = object(TGObject)
    priv: PWebKitNetworkRequestPrivate;
    function new(uri: Pgchar): PWebKitNetworkRequest; cdecl; inline; static;
    function get_message: PSoupMessage; cdecl; inline;
    function get_uri: Pgchar; cdecl; inline;
    procedure set_uri(uri: Pgchar); cdecl; inline;
    property message:  PSoupMessage read get_message  { property is writeable but setter not declared } ;
    property uri:  Pgchar read get_uri  { property is writeable but setter not declared } ;
  end;

  PPWebKitNetworkResponsePrivate = ^PWebKitNetworkResponsePrivate;
  PWebKitNetworkResponsePrivate = ^TWebKitNetworkResponsePrivate;
  TWebKitNetworkResponse = object(TGObject)
    priv: PWebKitNetworkResponsePrivate;
    function new(uri: Pgchar): PWebKitNetworkResponse; cdecl; inline; static;
    function get_message: PSoupMessage; cdecl; inline;
    function get_uri: Pgchar; cdecl; inline;
    procedure set_uri(uri: Pgchar); cdecl; inline;
    property message:  PSoupMessage read get_message  { property is writeable but setter not declared } ;
    property uri:  Pgchar read get_uri  { property is writeable but setter not declared } ;
  end;

  TWebKitDownloadPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitDownloadClass = ^PWebKitDownloadClass;
  PWebKitDownloadClass = ^TWebKitDownloadClass;
  TWebKitDownloadClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitDownloadError = ^PWebKitDownloadError;
  PWebKitDownloadError = ^TWebKitDownloadError;

  PPWebKitEditingBehavior = ^PWebKitEditingBehavior;
  PWebKitEditingBehavior = ^TWebKitEditingBehavior;

  PPWebKitGeolocationPolicyDecisionPrivate = ^PWebKitGeolocationPolicyDecisionPrivate;
  PWebKitGeolocationPolicyDecisionPrivate = ^TWebKitGeolocationPolicyDecisionPrivate;

  TWebKitGeolocationPolicyDecisionPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitGeolocationPolicyDecision = ^PWebKitGeolocationPolicyDecision;
  PWebKitGeolocationPolicyDecision = ^TWebKitGeolocationPolicyDecision;
  TWebKitGeolocationPolicyDecision = object(TGObject)
    priv: PWebKitGeolocationPolicyDecisionPrivate;
  end;

  PPWebKitGeolocationPolicyDecisionClass = ^PWebKitGeolocationPolicyDecisionClass;
  PWebKitGeolocationPolicyDecisionClass = ^TWebKitGeolocationPolicyDecisionClass;
  TWebKitGeolocationPolicyDecisionClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;
  TWebKitHitTestResultContext = packed object(TBitObject32)
  public
    property document: DWord index 2 read GetBit write SetBit;
    property link: DWord index 4 read GetBit write SetBit;
    property image: DWord index 8 read GetBit write SetBit;
    property media: DWord index 16 read GetBit write SetBit;
    property selection: DWord index 32 read GetBit write SetBit;
    property editable: DWord index 64 read GetBit write SetBit;
  end;


  PPWebKitHitTestResultPrivate = ^PWebKitHitTestResultPrivate;
  PWebKitHitTestResultPrivate = ^TWebKitHitTestResultPrivate;

  TWebKitHitTestResultPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitHitTestResult = ^PWebKitHitTestResult;
  PWebKitHitTestResult = ^TWebKitHitTestResult;
  TWebKitHitTestResult = object(TGObject)
    priv: PWebKitHitTestResultPrivate;
    //property context: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_context  { property is writeable but setter not declared } ;
    //property image_uri: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_image_uri  { property is writeable but setter not declared } ;
    //property inner_node: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_inner_node  { property is writeable but setter not declared } ;
    //property link_uri: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_link_uri  { property is writeable but setter not declared } ;
    //property media_uri: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_media_uri  { property is writeable but setter not declared } ;
  end;

  PPWebKitHitTestResultClass = ^PWebKitHitTestResultClass;
  PWebKitHitTestResultClass = ^TWebKitHitTestResultClass;
  TWebKitHitTestResultClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitIconDatabasePrivate = ^PWebKitIconDatabasePrivate;
  PWebKitIconDatabasePrivate = ^TWebKitIconDatabasePrivate;

  TWebKitIconDatabasePrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebFrame = ^PWebKitWebFrame;
  PWebKitWebFrame = ^TWebKitWebFrame;

  PPWebKitWebView = ^PWebKitWebView;
  PWebKitWebView = ^TWebKitWebView;

  PPWebKitWebDataSource = ^PWebKitWebDataSource;
  PWebKitWebDataSource = ^TWebKitWebDataSource;

  PPWebKitLoadStatus = ^PWebKitLoadStatus;
  PWebKitLoadStatus = ^TWebKitLoadStatus;

  PPWebKitSecurityOrigin = ^PWebKitSecurityOrigin;
  PWebKitSecurityOrigin = ^TWebKitSecurityOrigin;

  PPWebKitWebFramePrivate = ^PWebKitWebFramePrivate;
  PWebKitWebFramePrivate = ^TWebKitWebFramePrivate;
  TWebKitWebFrame = object(TGObject)
    priv: PWebKitWebFramePrivate;
    function new(web_view: PWebKitWebView): PWebKitWebFrame; cdecl; inline; static;
    function find_frame(name: Pgchar): PWebKitWebFrame; cdecl; inline;
    function get_data_source: PWebKitWebDataSource; cdecl; inline;
    function get_global_context: TJSGlobalContextRef; cdecl; inline;
    function get_horizontal_scrollbar_policy: TGtkPolicyType; cdecl; inline;
    function get_load_status: TWebKitLoadStatus; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_network_response: PWebKitNetworkResponse; cdecl; inline;
    function get_parent: PWebKitWebFrame; cdecl; inline;
    function get_provisional_data_source: PWebKitWebDataSource; cdecl; inline;
    function get_security_origin: PWebKitSecurityOrigin; cdecl; inline;
    function get_title: Pgchar; cdecl; inline;
    function get_uri: Pgchar; cdecl; inline;
    function get_vertical_scrollbar_policy: TGtkPolicyType; cdecl; inline;
    function get_web_view: PWebKitWebView; cdecl; inline;
    procedure load_alternate_string(content: Pgchar; base_url: Pgchar; unreachable_url: Pgchar); cdecl; inline;
    procedure load_request(request: PWebKitNetworkRequest); cdecl; inline;
    procedure load_string(content: Pgchar; mime_type: Pgchar; encoding: Pgchar; base_uri: Pgchar); cdecl; inline;
    procedure load_uri(uri: Pgchar); cdecl; inline;
    procedure print; cdecl; inline;
    function print_full(operation: PGtkPrintOperation; action: TGtkPrintOperationAction): TGtkPrintOperationResult; cdecl; inline;
    procedure reload; cdecl; inline;
    procedure stop_loading; cdecl; inline;
    property horizontal_scrollbar_policy:  TGtkPolicyType read get_horizontal_scrollbar_policy ;
    property load_status:  TWebKitLoadStatus read get_load_status ;
    property name:  Pgchar read get_name ;
    property title:  Pgchar read get_title ;
    property uri:  Pgchar read get_uri ;
    property vertical_scrollbar_policy:  TGtkPolicyType read get_vertical_scrollbar_policy ;
  end;

  PPWebKitIconDatabase = ^PWebKitIconDatabase;
  PWebKitIconDatabase = ^TWebKitIconDatabase;
  TWebKitIconDatabase = object(TGObject)
    priv: PWebKitIconDatabasePrivate;
    procedure clear; cdecl; inline;
    function get_icon_pixbuf(page_uri: Pgchar): PGdkPixbuf; cdecl; inline;
    function get_icon_uri(page_uri: Pgchar): Pgchar; cdecl; inline;
    function get_path: Pgchar; cdecl; inline;
    procedure set_path(path: Pgchar); cdecl; inline;
    property path:  Pgchar read get_path  { property is writeable but setter not declared } ;
  end;

  PPWebKitIconDatabaseClass = ^PWebKitIconDatabaseClass;
  PWebKitIconDatabaseClass = ^TWebKitIconDatabaseClass;
  TWebKitIconDatabaseClass = object
    parent_class: TGObjectClass;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
    _webkit_reserved4: procedure; cdecl;
  end;

  PPWebKitInsertAction = ^PWebKitInsertAction;
  PWebKitInsertAction = ^TWebKitInsertAction;

  PPWebKitNavigationResponse = ^PWebKitNavigationResponse;
  PWebKitNavigationResponse = ^TWebKitNavigationResponse;

  PPWebKitNetworkError = ^PWebKitNetworkError;
  PWebKitNetworkError = ^TWebKitNetworkError;

  TWebKitNetworkRequestPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitNetworkRequestClass = ^PWebKitNetworkRequestClass;
  PWebKitNetworkRequestClass = ^TWebKitNetworkRequestClass;
  TWebKitNetworkRequestClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  TWebKitNetworkResponsePrivate = record
    Unknown: Pointer;
  end;



  PPWebKitNetworkResponseClass = ^PWebKitNetworkResponseClass;
  PWebKitNetworkResponseClass = ^TWebKitNetworkResponseClass;
  TWebKitNetworkResponseClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitPluginError = ^PWebKitPluginError;
  PWebKitPluginError = ^TWebKitPluginError;

  PPWebKitPolicyError = ^PWebKitPolicyError;
  PWebKitPolicyError = ^TWebKitPolicyError;

  PPWebKitSecurityOriginPrivate = ^PWebKitSecurityOriginPrivate;
  PWebKitSecurityOriginPrivate = ^TWebKitSecurityOriginPrivate;

  TWebKitSecurityOriginPrivate = record
    Unknown: Pointer;
  end;


  TWebKitSecurityOrigin = object(TGObject)
    priv: PWebKitSecurityOriginPrivate;
    function get_all_web_databases: PGList; cdecl; inline;
    function get_host: Pgchar; cdecl; inline;
    function get_port: guint; cdecl; inline;
    function get_protocol: Pgchar; cdecl; inline;
    function get_web_database_quota: guint64; cdecl; inline;
    function get_web_database_usage: guint64; cdecl; inline;
    procedure set_web_database_quota(quota: guint64); cdecl; inline;
    property host:  Pgchar read get_host ;
    property port:  guint read get_port ;
    property protocol:  Pgchar read get_protocol ;
    property web_database_quota:  guint64 read get_web_database_quota  { property is writeable but setter not declared } ;
    property web_database_usage:  guint64 read get_web_database_usage ;
  end;

  PPWebKitSecurityOriginClass = ^PWebKitSecurityOriginClass;
  PWebKitSecurityOriginClass = ^TWebKitSecurityOriginClass;
  TWebKitSecurityOriginClass = object
    parent_class: TGObjectClass;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
    _webkit_reserved4: procedure; cdecl;
  end;

  PPWebKitSelectionAffinity = ^PWebKitSelectionAffinity;
  PWebKitSelectionAffinity = ^TWebKitSelectionAffinity;

  PPWebKitSoupAuthDialog = ^PWebKitSoupAuthDialog;
  PWebKitSoupAuthDialog = ^TWebKitSoupAuthDialog;
  TWebKitSoupAuthDialog = object(TGObject)
  end;

  PPWebKitSoupAuthDialogClass = ^PWebKitSoupAuthDialogClass;
  PWebKitSoupAuthDialogClass = ^TWebKitSoupAuthDialogClass;
  TWebKitSoupAuthDialogClass = object
    parent_class: TGObjectClass;
    current_toplevel: function(authDialog: PWebKitSoupAuthDialog; message: PSoupMessage): PGtkWidget; cdecl;
  end;

  PPWebKitViewportAttributesPrivate = ^PWebKitViewportAttributesPrivate;
  PWebKitViewportAttributesPrivate = ^TWebKitViewportAttributesPrivate;

  TWebKitViewportAttributesPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitViewportAttributes = ^PWebKitViewportAttributes;
  PWebKitViewportAttributes = ^TWebKitViewportAttributes;
  TWebKitViewportAttributes = object(TGObject)
    priv: PWebKitViewportAttributesPrivate;
    procedure recompute; cdecl; inline;
    //property available_height: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_available_height  { property is writeable but setter not declared } ;
    //property available_width: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_available_width  { property is writeable but setter not declared } ;
    //property desktop_width: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_desktop_width  { property is writeable but setter not declared } ;
    //property device_dpi: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_device_dpi  { property is writeable but setter not declared } ;
    //property device_height: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_device_height  { property is writeable but setter not declared } ;
    //property device_pixel_ratio: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_device_pixel_ratio ;
    //property device_width: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_device_width  { property is writeable but setter not declared } ;
    //property height: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_height ;
    //property initial_scale_factor: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_initial_scale_factor ;
    //property maximum_scale_factor: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_maximum_scale_factor ;
    //property minimum_scale_factor: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_minimum_scale_factor ;
    //property user_scalable: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_user_scalable ;
    //property valid: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_valid ;
    //property width: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_width ;
  end;

  PPWebKitViewportAttributesClass = ^PWebKitViewportAttributesClass;
  PWebKitViewportAttributesClass = ^TWebKitViewportAttributesClass;
  TWebKitViewportAttributesClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitWebBackForwardList = ^PWebKitWebBackForwardList;
  PWebKitWebBackForwardList = ^TWebKitWebBackForwardList;

  PPWebKitWebHistoryItem = ^PWebKitWebHistoryItem;
  PWebKitWebHistoryItem = ^TWebKitWebHistoryItem;

  PPWebKitWebHistoryItemPrivate = ^PWebKitWebHistoryItemPrivate;
  PWebKitWebHistoryItemPrivate = ^TWebKitWebHistoryItemPrivate;
  TWebKitWebHistoryItem = object(TGObject)
    priv: PWebKitWebHistoryItemPrivate;
    function new: PWebKitWebHistoryItem; cdecl; inline; static;
    function new_with_data(uri: Pgchar; title: Pgchar): PWebKitWebHistoryItem; cdecl; inline; static;
    function copy: PWebKitWebHistoryItem; cdecl; inline;
    function get_alternate_title: Pgchar; cdecl; inline;
    function get_last_visited_time: gdouble; cdecl; inline;
    function get_original_uri: Pgchar; cdecl; inline;
    function get_title: Pgchar; cdecl; inline;
    function get_uri: Pgchar; cdecl; inline;
    procedure set_alternate_title(title: Pgchar); cdecl; inline;
    property alternate_title:  Pgchar read get_alternate_title  { property is writeable but setter not declared } ;
    property last_visited_time:  gdouble read get_last_visited_time ;
    property original_uri:  Pgchar read get_original_uri ;
    property title:  Pgchar read get_title ;
    property uri:  Pgchar read get_uri ;
  end;

  PPWebKitWebBackForwardListPrivate = ^PWebKitWebBackForwardListPrivate;
  PWebKitWebBackForwardListPrivate = ^TWebKitWebBackForwardListPrivate;
  TWebKitWebBackForwardList = object(TGObject)
    priv: PWebKitWebBackForwardListPrivate;
    function new_with_web_view(web_view: PWebKitWebView): PWebKitWebBackForwardList; cdecl; inline; static;
    procedure add_item(history_item: TWebKitWebHistoryItem); cdecl; inline;
    procedure clear; cdecl; inline;
    function contains_item(history_item: TWebKitWebHistoryItem): gboolean; cdecl; inline;
    function get_back_item: TWebKitWebHistoryItem; cdecl; inline;
    function get_back_length: gint; cdecl; inline;
    function get_back_list_with_limit(limit: gint): PGList; cdecl; inline;
    function get_current_item: TWebKitWebHistoryItem; cdecl; inline;
    function get_forward_item: TWebKitWebHistoryItem; cdecl; inline;
    function get_forward_length: gint; cdecl; inline;
    function get_forward_list_with_limit(limit: gint): PGList; cdecl; inline;
    function get_limit: gint; cdecl; inline;
    function get_nth_item(index: gint): TWebKitWebHistoryItem; cdecl; inline;
    procedure go_back; cdecl; inline;
    procedure go_forward; cdecl; inline;
    procedure go_to_item(history_item: TWebKitWebHistoryItem); cdecl; inline;
    procedure set_limit(limit: gint); cdecl; inline;
  end;

  PPWebKitWebInspector = ^PWebKitWebInspector;
  PWebKitWebInspector = ^TWebKitWebInspector;

  PPWebKitWebSettings = ^PWebKitWebSettings;
  PWebKitWebSettings = ^TWebKitWebSettings;

  PPWebKitWebViewViewMode = ^PWebKitWebViewViewMode;
  PWebKitWebViewViewMode = ^TWebKitWebViewViewMode;

  PPWebKitWebWindowFeatures = ^PWebKitWebWindowFeatures;
  PWebKitWebWindowFeatures = ^TWebKitWebWindowFeatures;

  PPWebKitWebViewPrivate = ^PWebKitWebViewPrivate;
  PWebKitWebViewPrivate = ^TWebKitWebViewPrivate;
  TWebKitWebView = object(TGtkContainer)
    priv2: PWebKitWebViewPrivate;
    function new: PWebKitWebView; cdecl; inline; static;
    function can_copy_clipboard: gboolean; cdecl; inline;
    function can_cut_clipboard: gboolean; cdecl; inline;
    function can_go_back: gboolean; cdecl; inline;
    function can_go_back_or_forward(steps: gint): gboolean; cdecl; inline;
    function can_go_forward: gboolean; cdecl; inline;
    function can_paste_clipboard: gboolean; cdecl; inline;
    function can_redo: gboolean; cdecl; inline;
    function can_show_mime_type(mime_type: Pgchar): gboolean; cdecl; inline;
    function can_undo: gboolean; cdecl; inline;
    procedure copy_clipboard; cdecl; inline;
    procedure cut_clipboard; cdecl; inline;
    procedure delete_selection; cdecl; inline;
    procedure execute_script(script: Pgchar); cdecl; inline;
    function get_back_forward_list: PWebKitWebBackForwardList; cdecl; inline;
    function get_copy_target_list: PGtkTargetList; cdecl; inline;
    function get_custom_encoding: Pgchar; cdecl; inline;
    function get_dom_document: PWebKitDOMDocument; cdecl; inline;
    function get_editable: gboolean; cdecl; inline;
    function get_encoding: Pgchar; cdecl; inline;
    function get_focused_frame: PWebKitWebFrame; cdecl; inline;
    function get_full_content_zoom: gboolean; cdecl; inline;
    function get_hit_test_result(event: PGdkEventButton): PWebKitHitTestResult; cdecl; inline;
    function get_icon_pixbuf: PGdkPixbuf; cdecl; inline;
    function get_icon_uri: Pgchar; cdecl; inline;
    function get_inspector: PWebKitWebInspector; cdecl; inline;
    function get_load_status: TWebKitLoadStatus; cdecl; inline;
    function get_main_frame: PWebKitWebFrame; cdecl; inline;
    function get_paste_target_list: PGtkTargetList; cdecl; inline;
    function get_progress: gdouble; cdecl; inline;
    function get_settings: PWebKitWebSettings; cdecl; inline;
    function get_title: Pgchar; cdecl; inline;
    function get_transparent: gboolean; cdecl; inline;
    function get_uri: Pgchar; cdecl; inline;
    function get_view_mode: TWebKitWebViewViewMode; cdecl; inline;
    function get_view_source_mode: gboolean; cdecl; inline;
    function get_viewport_attributes: PWebKitViewportAttributes; cdecl; inline;
    function get_window_features: PWebKitWebWindowFeatures; cdecl; inline;
    function get_zoom_level: gfloat; cdecl; inline;
    procedure go_back; cdecl; inline;
    procedure go_back_or_forward(steps: gint); cdecl; inline;
    procedure go_forward; cdecl; inline;
    function go_to_back_forward_item(item: PWebKitWebHistoryItem): gboolean; cdecl; inline;
    function has_selection: gboolean; cdecl; inline;
    procedure load_html_string(content: Pgchar; base_uri: Pgchar); cdecl; inline;
    procedure load_request(request: PWebKitNetworkRequest); cdecl; inline;
    procedure load_string(content: Pgchar; mime_type: Pgchar; encoding: Pgchar; base_uri: Pgchar); cdecl; inline;
    procedure load_uri(uri: Pgchar); cdecl; inline;
    function mark_text_matches(string_: Pgchar; case_sensitive: gboolean; limit: guint): guint; cdecl; inline;
    procedure move_cursor(step: TGtkMovementStep; count: gint); cdecl; inline;
    procedure open(uri: Pgchar); cdecl; inline;
    procedure paste_clipboard; cdecl; inline;
    procedure redo; cdecl; inline;
    procedure reload; cdecl; inline;
    procedure reload_bypass_cache; cdecl; inline;
    function search_text(text: Pgchar; case_sensitive: gboolean; forward: gboolean; wrap: gboolean): gboolean; cdecl; inline;
    procedure select_all; cdecl; inline;
    procedure set_custom_encoding(encoding: Pgchar); cdecl; inline;
    procedure set_editable(flag: gboolean); cdecl; inline;
    procedure set_full_content_zoom(full_content_zoom: gboolean); cdecl; inline;
    procedure set_highlight_text_matches(highlight: gboolean); cdecl; inline;
    procedure set_maintains_back_forward_list(flag: gboolean); cdecl; inline;
    procedure set_settings(settings: PWebKitWebSettings); cdecl; inline;
    procedure set_transparent(flag: gboolean); cdecl; inline;
    procedure set_view_mode(mode: TWebKitWebViewViewMode); cdecl; inline;
    procedure set_view_source_mode(view_source_mode: gboolean); cdecl; inline;
    procedure set_zoom_level(zoom_level: gfloat); cdecl; inline;
    procedure stop_loading; cdecl; inline;
    procedure undo; cdecl; inline;
    procedure unmark_text_matches; cdecl; inline;
    procedure zoom_in; cdecl; inline;
    procedure zoom_out; cdecl; inline;
    property copy_target_list:  PGtkTargetList read get_copy_target_list ;
    property custom_encoding:  Pgchar read get_custom_encoding  { property is writeable but setter not declared } ;
    property editable:  gboolean read get_editable  { property is writeable but setter not declared } ;
    property encoding:  Pgchar read get_encoding ;
    property full_content_zoom:  gboolean read get_full_content_zoom  { property is writeable but setter not declared } ;
    property icon_uri:  Pgchar read get_icon_uri ;
    //property im_context: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_im_context ;
    property load_status:  TWebKitLoadStatus read get_load_status ;
    property paste_target_list:  PGtkTargetList read get_paste_target_list ;
    property progress:  gdouble read get_progress ;
    property settings:  PWebKitWebSettings read get_settings  { property is writeable but setter not declared } ;
    property title:  Pgchar read get_title ;
    property transparent:  gboolean read get_transparent  { property is writeable but setter not declared } ;
    property uri:  Pgchar read get_uri ;
    property view_mode:  TWebKitWebViewViewMode read get_view_mode  { property is writeable but setter not declared } ;
    property viewport_attributes:  PWebKitViewportAttributes read get_viewport_attributes ;
    //property web_inspector: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_web_inspector ;
    property window_features:  PWebKitWebWindowFeatures read get_window_features  { property is writeable but setter not declared } ;
    property zoom_level:  gfloat read get_zoom_level  { property is writeable but setter not declared } ;
  end;

  TWebKitWebBackForwardListPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebBackForwardListClass = ^PWebKitWebBackForwardListClass;
  PWebKitWebBackForwardListClass = ^TWebKitWebBackForwardListClass;
  TWebKitWebBackForwardListClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitWebResource = ^PWebKitWebResource;
  PWebKitWebResource = ^TWebKitWebResource;

  PPWebKitWebDataSourcePrivate = ^PWebKitWebDataSourcePrivate;
  PWebKitWebDataSourcePrivate = ^TWebKitWebDataSourcePrivate;
  TWebKitWebDataSource = object(TGObject)
    priv: PWebKitWebDataSourcePrivate;
    function new: PWebKitWebDataSource; cdecl; inline; static;
    function new_with_request(request: PWebKitNetworkRequest): PWebKitWebDataSource; cdecl; inline; static;
    function get_data: PGString; cdecl; inline;
    function get_encoding: Pgchar; cdecl; inline;
    function get_initial_request: PWebKitNetworkRequest; cdecl; inline;
    function get_main_resource: PWebKitWebResource; cdecl; inline;
    function get_request: PWebKitNetworkRequest; cdecl; inline;
    function get_subresources: PGList; cdecl; inline;
    function get_unreachable_uri: Pgchar; cdecl; inline;
    function get_web_frame: PWebKitWebFrame; cdecl; inline;
    function is_loading: gboolean; cdecl; inline;
  end;

  PPWebKitWebResourcePrivate = ^PWebKitWebResourcePrivate;
  PWebKitWebResourcePrivate = ^TWebKitWebResourcePrivate;
  TWebKitWebResource = object(TGObject)
    priv: PWebKitWebResourcePrivate;
    function new(data: Pgchar; size: gssize; uri: Pgchar; mime_type: Pgchar; encoding: Pgchar; frame_name: Pgchar): PWebKitWebResource; cdecl; inline; static;
    function get_data: PGString; cdecl; inline;
    function get_encoding: Pgchar; cdecl; inline;
    function get_frame_name: Pgchar; cdecl; inline;
    function get_mime_type: Pgchar; cdecl; inline;
    function get_uri: Pgchar; cdecl; inline;
    property encoding:  Pgchar read get_encoding ;
    property frame_name:  Pgchar read get_frame_name ;
    property mime_type:  Pgchar read get_mime_type ;
    property uri:  Pgchar read get_uri  { property is writeable but setter not declared } ;
  end;

  TWebKitWebDataSourcePrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebDataSourceClass = ^PWebKitWebDataSourceClass;
  PWebKitWebDataSourceClass = ^TWebKitWebDataSourceClass;
  TWebKitWebDataSourceClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitWebDatabasePrivate = ^PWebKitWebDatabasePrivate;
  PWebKitWebDatabasePrivate = ^TWebKitWebDatabasePrivate;

  TWebKitWebDatabasePrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebDatabase = ^PWebKitWebDatabase;
  PWebKitWebDatabase = ^TWebKitWebDatabase;
  TWebKitWebDatabase = object(TGObject)
    priv: PWebKitWebDatabasePrivate;
    function get_display_name: Pgchar; cdecl; inline;
    function get_expected_size: guint64; cdecl; inline;
    function get_filename: Pgchar; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_security_origin: PWebKitSecurityOrigin; cdecl; inline;
    function get_size: guint64; cdecl; inline;
    procedure remove; cdecl; inline;
    property display_name:  Pgchar read get_display_name ;
    property expected_size:  guint64 read get_expected_size ;
    property filename:  Pgchar read get_filename ;
    property name:  Pgchar read get_name  { property is writeable but setter not declared } ;
    property security_origin:  PWebKitSecurityOrigin read get_security_origin  { property is writeable but setter not declared } ;
    property size:  guint64 read get_size ;
  end;

  PPWebKitWebDatabaseClass = ^PWebKitWebDatabaseClass;
  PWebKitWebDatabaseClass = ^TWebKitWebDatabaseClass;
  TWebKitWebDatabaseClass = object
    parent_class: TGObjectClass;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
    _webkit_reserved4: procedure; cdecl;
  end;

  TWebKitWebFramePrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebFrameClass = ^PWebKitWebFrameClass;
  PWebKitWebFrameClass = ^TWebKitWebFrameClass;
  TWebKitWebFrameClass = object
    parent_class: TGObjectClass;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
    _webkit_reserved4: procedure; cdecl;
    _webkit_reserved5: procedure; cdecl;
    _webkit_reserved6: procedure; cdecl;
  end;

  TWebKitWebHistoryItemPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebHistoryItemClass = ^PWebKitWebHistoryItemClass;
  PWebKitWebHistoryItemClass = ^TWebKitWebHistoryItemClass;
  TWebKitWebHistoryItemClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitWebInspectorPrivate = ^PWebKitWebInspectorPrivate;
  PWebKitWebInspectorPrivate = ^TWebKitWebInspectorPrivate;

  TWebKitWebInspectorPrivate = record
    Unknown: Pointer;
  end;


  TWebKitWebInspector = object(TGObject)
    priv: PWebKitWebInspectorPrivate;
    procedure close; cdecl; inline;
    function get_inspected_uri: Pgchar; cdecl; inline;
    function get_web_view: PWebKitWebView; cdecl; inline;
    procedure inspect_coordinates(x: gdouble; y: gdouble); cdecl; inline;
    procedure inspect_node(node: PWebKitDOMNode); cdecl; inline;
    procedure show; cdecl; inline;
    property inspected_uri:  Pgchar read get_inspected_uri ;
    //property javascript_profiling_enabled: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_javascript_profiling_enabled  { property is writeable but setter not declared } ;
    //property timeline_profiling_enabled: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_timeline_profiling_enabled  { property is writeable but setter not declared } ;
    property web_view:  PWebKitWebView read get_web_view ;
  end;

  PPWebKitWebInspectorClass = ^PWebKitWebInspectorClass;
  PWebKitWebInspectorClass = ^TWebKitWebInspectorClass;
  TWebKitWebInspectorClass = object
    parent_class: TGObjectClass;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
    _webkit_reserved4: procedure; cdecl;
  end;

  PPWebKitWebNavigationReason = ^PWebKitWebNavigationReason;
  PWebKitWebNavigationReason = ^TWebKitWebNavigationReason;

  PPWebKitWebNavigationActionPrivate = ^PWebKitWebNavigationActionPrivate;
  PWebKitWebNavigationActionPrivate = ^TWebKitWebNavigationActionPrivate;

  TWebKitWebNavigationActionPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebNavigationAction = ^PWebKitWebNavigationAction;
  PWebKitWebNavigationAction = ^TWebKitWebNavigationAction;
  TWebKitWebNavigationAction = object(TGObject)
    priv: PWebKitWebNavigationActionPrivate;
    function get_button: gint; cdecl; inline;
    function get_modifier_state: gint; cdecl; inline;
    function get_original_uri: Pgchar; cdecl; inline;
    function get_reason: TWebKitWebNavigationReason; cdecl; inline;
    function get_target_frame: Pgchar; cdecl; inline;
    procedure set_original_uri(originalUri: Pgchar); cdecl; inline;
    procedure set_reason(reason: TWebKitWebNavigationReason); cdecl; inline;
    property button:  gint read get_button  { property is writeable but setter not declared } ;
    property modifier_state:  gint read get_modifier_state  { property is writeable but setter not declared } ;
    property original_uri:  Pgchar read get_original_uri  { property is writeable but setter not declared } ;
    property reason:  TWebKitWebNavigationReason read get_reason  { property is writeable but setter not declared } ;
    property target_frame:  Pgchar read get_target_frame  { property is writeable but setter not declared } ;
  end;

  PPWebKitWebNavigationActionClass = ^PWebKitWebNavigationActionClass;
  PWebKitWebNavigationActionClass = ^TWebKitWebNavigationActionClass;
  TWebKitWebNavigationActionClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitWebPluginPrivate = ^PWebKitWebPluginPrivate;
  PWebKitWebPluginPrivate = ^TWebKitWebPluginPrivate;

  TWebKitWebPluginPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebPlugin = ^PWebKitWebPlugin;
  PWebKitWebPlugin = ^TWebKitWebPlugin;
  TWebKitWebPlugin = object(TGObject)
    priv: PWebKitWebPluginPrivate;
    function get_description: Pgchar; cdecl; inline;
    function get_enabled: gboolean; cdecl; inline;
    function get_mimetypes: PGSList; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_path: Pgchar; cdecl; inline;
    procedure set_enabled(param0: gboolean); cdecl; inline;
    property enabled:  gboolean read get_enabled  { property is writeable but setter not declared } ;
  end;

  PPWebKitWebPluginClass = ^PWebKitWebPluginClass;
  PWebKitWebPluginClass = ^TWebKitWebPluginClass;
  TWebKitWebPluginClass = object
    parentClass: TGObjectClass;
  end;

  PPWebKitWebPluginDatabasePrivate = ^PWebKitWebPluginDatabasePrivate;
  PWebKitWebPluginDatabasePrivate = ^TWebKitWebPluginDatabasePrivate;

  TWebKitWebPluginDatabasePrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebPluginDatabase = ^PWebKitWebPluginDatabase;
  PWebKitWebPluginDatabase = ^TWebKitWebPluginDatabase;
  TWebKitWebPluginDatabase = object(TGObject)
    priv: PWebKitWebPluginDatabasePrivate;
    procedure plugins_list_free(param0: PGSList); cdecl; inline; static;
    function get_plugin_for_mimetype(param0: Pgchar): PWebKitWebPlugin; cdecl; inline;
    function get_plugins: PGSList; cdecl; inline;
    procedure refresh; cdecl; inline;
  end;

  PPWebKitWebPluginDatabaseClass = ^PWebKitWebPluginDatabaseClass;
  PWebKitWebPluginDatabaseClass = ^TWebKitWebPluginDatabaseClass;
  TWebKitWebPluginDatabaseClass = object
    parentClass: TGObjectClass;
  end;

  PPWebKitWebPolicyDecisionPrivate = ^PWebKitWebPolicyDecisionPrivate;
  PWebKitWebPolicyDecisionPrivate = ^TWebKitWebPolicyDecisionPrivate;

  TWebKitWebPolicyDecisionPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebPolicyDecision = ^PWebKitWebPolicyDecision;
  PWebKitWebPolicyDecision = ^TWebKitWebPolicyDecision;
  TWebKitWebPolicyDecision = object(TGObject)
    priv: PWebKitWebPolicyDecisionPrivate;
    procedure download; cdecl; inline;
    procedure ignore; cdecl; inline;
    procedure use; cdecl; inline;
  end;

  PPWebKitWebPolicyDecisionClass = ^PWebKitWebPolicyDecisionClass;
  PWebKitWebPolicyDecisionClass = ^TWebKitWebPolicyDecisionClass;
  TWebKitWebPolicyDecisionClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  TWebKitWebResourcePrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebResourceClass = ^PWebKitWebResourceClass;
  PWebKitWebResourceClass = ^TWebKitWebResourceClass;
  TWebKitWebResourceClass = object
    parent_class: TGObjectClass;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
  end;

  PPWebKitWebSettingsPrivate = ^PWebKitWebSettingsPrivate;
  PWebKitWebSettingsPrivate = ^TWebKitWebSettingsPrivate;
  TWebKitWebSettings = object(TGObject)
    priv: PWebKitWebSettingsPrivate;
    function new: PWebKitWebSettings; cdecl; inline; static;
    function copy: PWebKitWebSettings; cdecl; inline;
    function get_user_agent: Pgchar; cdecl; inline;
    //property auto_load_images: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_auto_load_images  { property is writeable but setter not declared } ;
    //property auto_resize_window: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_auto_resize_window  { property is writeable but setter not declared } ;
    //property auto_shrink_images: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_auto_shrink_images  { property is writeable but setter not declared } ;
    //property cursive_font_family: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_cursive_font_family  { property is writeable but setter not declared } ;
    //property default_encoding: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_default_encoding  { property is writeable but setter not declared } ;
    //property default_font_family: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_default_font_family  { property is writeable but setter not declared } ;
    //property default_font_size: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_default_font_size  { property is writeable but setter not declared } ;
    //property default_monospace_font_size: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_default_monospace_font_size  { property is writeable but setter not declared } ;
    //property editing_behavior: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_editing_behavior  { property is writeable but setter not declared } ;
    //property enable_caret_browsing: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_caret_browsing  { property is writeable but setter not declared } ;
    //property enable_default_context_menu: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_default_context_menu  { property is writeable but setter not declared } ;
    //property enable_developer_extras: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_developer_extras  { property is writeable but setter not declared } ;
    //property enable_dns_prefetching: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_dns_prefetching  { property is writeable but setter not declared } ;
    //property enable_dom_paste: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_dom_paste  { property is writeable but setter not declared } ;
    //property enable_file_access_from_file_uris: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_file_access_from_file_uris  { property is writeable but setter not declared } ;
    //property enable_frame_flattening: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_frame_flattening  { property is writeable but setter not declared } ;
    //property enable_fullscreen: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_fullscreen  { property is writeable but setter not declared } ;
    //property enable_html5_database: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_html5_database  { property is writeable but setter not declared } ;
    //property enable_html5_local_storage: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_html5_local_storage  { property is writeable but setter not declared } ;
    //property enable_hyperlink_auditing: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_hyperlink_auditing  { property is writeable but setter not declared } ;
    //property enable_java_applet: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_java_applet  { property is writeable but setter not declared } ;
    //property enable_offline_web_application_cache: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_offline_web_application_cache  { property is writeable but setter not declared } ;
    //property enable_page_cache: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_page_cache  { property is writeable but setter not declared } ;
    //property enable_plugins: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_plugins  { property is writeable but setter not declared } ;
    //property enable_private_browsing: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_private_browsing  { property is writeable but setter not declared } ;
    //property enable_scripts: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_scripts  { property is writeable but setter not declared } ;
    //property enable_site_specific_quirks: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_site_specific_quirks  { property is writeable but setter not declared } ;
    //property enable_spatial_navigation: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_spatial_navigation  { property is writeable but setter not declared } ;
    //property enable_spell_checking: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_spell_checking  { property is writeable but setter not declared } ;
    //property enable_universal_access_from_file_uris: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_universal_access_from_file_uris  { property is writeable but setter not declared } ;
    //property enable_xss_auditor: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enable_xss_auditor  { property is writeable but setter not declared } ;
    //property enforce_96_dpi: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_enforce_96_dpi  { property is writeable but setter not declared } ;
    //property fantasy_font_family: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_fantasy_font_family  { property is writeable but setter not declared } ;
    //property javascript_can_access_clipboard: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_javascript_can_access_clipboard  { property is writeable but setter not declared } ;
    //property javascript_can_open_windows_automatically: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_javascript_can_open_windows_automatically  { property is writeable but setter not declared } ;
    //property minimum_font_size: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_minimum_font_size  { property is writeable but setter not declared } ;
    //property minimum_logical_font_size: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_minimum_logical_font_size  { property is writeable but setter not declared } ;
    //property monospace_font_family: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_monospace_font_family  { property is writeable but setter not declared } ;
    //property print_backgrounds: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_print_backgrounds  { property is writeable but setter not declared } ;
    //property resizable_text_areas: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_resizable_text_areas  { property is writeable but setter not declared } ;
    //property sans_serif_font_family: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_sans_serif_font_family  { property is writeable but setter not declared } ;
    //property serif_font_family: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_serif_font_family  { property is writeable but setter not declared } ;
    //property spell_checking_languages: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_spell_checking_languages  { property is writeable but setter not declared } ;
    //property tab_key_cycles_through_elements: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_tab_key_cycles_through_elements  { property is writeable but setter not declared } ;
    property user_agent:  Pgchar read get_user_agent  { property is writeable but setter not declared } ;
    //property user_stylesheet_uri: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_user_stylesheet_uri  { property is writeable but setter not declared } ;
    //property zoom_step: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_zoom_step  { property is writeable but setter not declared } ;
  end;

  TWebKitWebSettingsPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebSettingsClass = ^PWebKitWebSettingsClass;
  PWebKitWebSettingsClass = ^TWebKitWebSettingsClass;
  TWebKitWebSettingsClass = object
    parent_class: TGObjectClass;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
    _webkit_reserved4: procedure; cdecl;
  end;

  PPWebKitWebWindowFeaturesPrivate = ^PWebKitWebWindowFeaturesPrivate;
  PWebKitWebWindowFeaturesPrivate = ^TWebKitWebWindowFeaturesPrivate;
  TWebKitWebWindowFeatures = object(TGObject)
    priv: PWebKitWebWindowFeaturesPrivate;
    function new: PWebKitWebWindowFeatures; cdecl; inline; static;
    function equal(features2: PWebKitWebWindowFeatures): gboolean; cdecl; inline;
    //property fullscreen: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_fullscreen  { property is writeable but setter not declared } ;
    //property height: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_height  { property is writeable but setter not declared } ;
    //property locationbar_visible: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_locationbar_visible  { property is writeable but setter not declared } ;
    //property menubar_visible: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_menubar_visible  { property is writeable but setter not declared } ;
    //property scrollbar_visible: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_scrollbar_visible  { property is writeable but setter not declared } ;
    //property statusbar_visible: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_statusbar_visible  { property is writeable but setter not declared } ;
    //property toolbar_visible: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_toolbar_visible  { property is writeable but setter not declared } ;
    //property width: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_width  { property is writeable but setter not declared } ;
    //property x: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_x  { property is writeable but setter not declared } ;
    //property y: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_y  { property is writeable but setter not declared } ;
  end;

  TWebKitWebViewPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebViewClass = ^PWebKitWebViewClass;
  PWebKitWebViewClass = ^TWebKitWebViewClass;
  TWebKitWebViewClass = object
    parent_class: TGtkContainerClass;
    create_web_view: function(web_view: PWebKitWebView; web_frame: PWebKitWebFrame): PWebKitWebView; cdecl;
    web_view_ready: function(web_view: PWebKitWebView): gboolean; cdecl;
    close_web_view: function(web_view: PWebKitWebView): gboolean; cdecl;
    navigation_requested: function(web_view: PWebKitWebView; frame: PWebKitWebFrame; request: PWebKitNetworkRequest): TWebKitNavigationResponse; cdecl;
    window_object_cleared: procedure(web_view: PWebKitWebView; frame: PWebKitWebFrame; context: TJSGlobalContextRef; window_object: TJSObjectRef); cdecl;
    choose_file: function(web_view: PWebKitWebView; frame: PWebKitWebFrame; old_file: Pgchar): Pgchar; cdecl;
    script_alert: function(web_view: PWebKitWebView; frame: PWebKitWebFrame; alert_message: Pgchar): gboolean; cdecl;
    script_confirm: function(web_view: PWebKitWebView; frame: PWebKitWebFrame; confirm_message: Pgchar; did_confirm: Pgboolean): gboolean; cdecl;
    script_prompt: function(web_view: PWebKitWebView; frame: PWebKitWebFrame; message: Pgchar; default_value: Pgchar; value: PPgchar): gboolean; cdecl;
    console_message: function(web_view: PWebKitWebView; message: Pgchar; line_number: guint; source_id: Pgchar): gboolean; cdecl;
    select_all: procedure(web_view: PWebKitWebView); cdecl;
    cut_clipboard: procedure(web_view: PWebKitWebView); cdecl;
    copy_clipboard: procedure(web_view: PWebKitWebView); cdecl;
    paste_clipboard: procedure(web_view: PWebKitWebView); cdecl;
    move_cursor: function(web_view: PWebKitWebView; step: TGtkMovementStep; count: gint): gboolean; cdecl;
    set_scroll_adjustments: procedure(web_view: PWebKitWebView; hadjustment: PGtkAdjustment; vadjustment: PGtkAdjustment); cdecl;
    undo: procedure(web_view: PWebKitWebView); cdecl;
    redo: procedure(web_view: PWebKitWebView); cdecl;
    should_allow_editing_action: function(web_view: PWebKitWebView): gboolean; cdecl;
    _webkit_reserved0: procedure; cdecl;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
  end;

  PPWebKitWebViewTargetInfo = ^PWebKitWebViewTargetInfo;
  PWebKitWebViewTargetInfo = ^TWebKitWebViewTargetInfo;

  TWebKitWebWindowFeaturesPrivate = record
    Unknown: Pointer;
  end;



  PPWebKitWebWindowFeaturesClass = ^PWebKitWebWindowFeaturesClass;
  PWebKitWebWindowFeaturesClass = ^TWebKitWebWindowFeaturesClass;
  TWebKitWebWindowFeaturesClass = object
    parent_class: TGObjectClass;
    _webkit_reserved1: procedure; cdecl;
    _webkit_reserved2: procedure; cdecl;
    _webkit_reserved3: procedure; cdecl;
    _webkit_reserved4: procedure; cdecl;
  end;

  PP_WebKitWebPluginMIMEType = ^P_WebKitWebPluginMIMEType;
  P_WebKitWebPluginMIMEType = ^T_WebKitWebPluginMIMEType;

  T_WebKitWebPluginMIMEType = record
    name: Pgchar;
    description: Pgchar;
    extensions: PPgchar;
  end;



function webkit_application_cache_get_database_directory_path: Pgchar; cdecl; external;
function webkit_application_cache_get_maximum_size: unsigned_long_long; cdecl; external;
function webkit_check_version(major: guint; minor: guint; micro: guint): gboolean; cdecl; external;
function webkit_dom_attr_get_is_id(ADOMAttr: PWebKitDOMAttr): gboolean; cdecl; external;
function webkit_dom_attr_get_name(ADOMAttr: PWebKitDOMAttr): Pgchar; cdecl; external;
function webkit_dom_attr_get_owner_element(ADOMAttr: PWebKitDOMAttr): PWebKitDOMElement; cdecl; external;
function webkit_dom_attr_get_specified(ADOMAttr: PWebKitDOMAttr): gboolean; cdecl; external;
function webkit_dom_attr_get_type: TGType; cdecl; external;
function webkit_dom_attr_get_value(ADOMAttr: PWebKitDOMAttr): Pgchar; cdecl; external;
function webkit_dom_bar_info_get_type: TGType; cdecl; external;
function webkit_dom_bar_info_get_visible(ADOMBarInfo: PWebKitDOMBarInfo): gboolean; cdecl; external;
function webkit_dom_blob_get_size(ADOMBlob: PWebKitDOMBlob): guint64; cdecl; external;
function webkit_dom_blob_get_type: TGType; cdecl; external;
function webkit_dom_blob_slice(ADOMBlob: PWebKitDOMBlob; start: gint64; length: gint64; content_type: Pgchar): PWebKitDOMBlob; cdecl; external;
function webkit_dom_cdata_section_get_type: TGType; cdecl; external;
function webkit_dom_character_data_get_data(ADOMCharacterData: PWebKitDOMCharacterData): Pgchar; cdecl; external;
function webkit_dom_character_data_get_length(ADOMCharacterData: PWebKitDOMCharacterData): gulong; cdecl; external;
function webkit_dom_character_data_get_type: TGType; cdecl; external;
function webkit_dom_character_data_substring_data(ADOMCharacterData: PWebKitDOMCharacterData; offset: gulong; length: gulong): Pgchar; cdecl; external;
function webkit_dom_comment_get_type: TGType; cdecl; external;
function webkit_dom_console_get_memory(ADOMConsole: PWebKitDOMConsole): PWebKitDOMMemoryInfo; cdecl; external;
function webkit_dom_console_get_type: TGType; cdecl; external;
function webkit_dom_css_rule_get_css_text(ADOMCSSRule: PWebKitDOMCSSRule): Pgchar; cdecl; external;
function webkit_dom_css_rule_get_parent_rule(ADOMCSSRule: PWebKitDOMCSSRule): PWebKitDOMCSSRule; cdecl; external;
function webkit_dom_css_rule_get_parent_style_sheet(ADOMCSSRule: PWebKitDOMCSSRule): PWebKitDOMCSSStyleSheet; cdecl; external;
function webkit_dom_css_rule_get_type: TGType; cdecl; external;
function webkit_dom_css_rule_list_get_length(ADOMCSSRuleList: PWebKitDOMCSSRuleList): gulong; cdecl; external;
function webkit_dom_css_rule_list_get_type: TGType; cdecl; external;
function webkit_dom_css_rule_list_item(ADOMCSSRuleList: PWebKitDOMCSSRuleList; index: gulong): PWebKitDOMCSSRule; cdecl; external;
function webkit_dom_css_style_declaration_get_css_text(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration): Pgchar; cdecl; external;
function webkit_dom_css_style_declaration_get_length(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration): gulong; cdecl; external;
function webkit_dom_css_style_declaration_get_parent_rule(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration): PWebKitDOMCSSRule; cdecl; external;
function webkit_dom_css_style_declaration_get_property_css_value(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; property_name: Pgchar): PWebKitDOMCSSValue; cdecl; external;
function webkit_dom_css_style_declaration_get_property_priority(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; property_name: Pgchar): Pgchar; cdecl; external;
function webkit_dom_css_style_declaration_get_property_shorthand(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; property_name: Pgchar): Pgchar; cdecl; external;
function webkit_dom_css_style_declaration_get_property_value(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; property_name: Pgchar): Pgchar; cdecl; external;
function webkit_dom_css_style_declaration_get_type: TGType; cdecl; external;
function webkit_dom_css_style_declaration_is_property_implicit(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; property_name: Pgchar): gboolean; cdecl; external;
function webkit_dom_css_style_declaration_item(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; index: gulong): Pgchar; cdecl; external;
function webkit_dom_css_style_declaration_remove_property(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; property_name: Pgchar): Pgchar; cdecl; external;
function webkit_dom_css_style_sheet_add_rule(ADOMCSSStyleSheet: PWebKitDOMCSSStyleSheet; selector: Pgchar; style: Pgchar; index: gulong): glong; cdecl; external;
function webkit_dom_css_style_sheet_get_css_rules(ADOMCSSStyleSheet: PWebKitDOMCSSStyleSheet): PWebKitDOMCSSRuleList; cdecl; external;
function webkit_dom_css_style_sheet_get_owner_rule(ADOMCSSStyleSheet: PWebKitDOMCSSStyleSheet): PWebKitDOMCSSRule; cdecl; external;
function webkit_dom_css_style_sheet_get_rules(ADOMCSSStyleSheet: PWebKitDOMCSSStyleSheet): PWebKitDOMCSSRuleList; cdecl; external;
function webkit_dom_css_style_sheet_get_type: TGType; cdecl; external;
function webkit_dom_css_style_sheet_insert_rule(ADOMCSSStyleSheet: PWebKitDOMCSSStyleSheet; rule: Pgchar; index: gulong): gulong; cdecl; external;
function webkit_dom_css_value_get_css_text(ADOMCSSValue: PWebKitDOMCSSValue): Pgchar; cdecl; external;
function webkit_dom_css_value_get_css_value_type(ADOMCSSValue: PWebKitDOMCSSValue): gushort; cdecl; external;
function webkit_dom_css_value_get_type: TGType; cdecl; external;
function webkit_dom_database_get_type: TGType; cdecl; external;
function webkit_dom_database_get_version(ADOMDatabase: PWebKitDOMDatabase): Pgchar; cdecl; external;
function webkit_dom_document_adopt_node(ADOMDocument: PWebKitDOMDocument; source: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_document_caret_range_from_point(ADOMDocument: PWebKitDOMDocument; x: glong; y: glong): PWebKitDOMRange; cdecl; external;
function webkit_dom_document_create_attribute(ADOMDocument: PWebKitDOMDocument; name: Pgchar): PWebKitDOMAttr; cdecl; external;
function webkit_dom_document_create_attribute_ns(ADOMDocument: PWebKitDOMDocument; namespace_uri: Pgchar; qualified_name: Pgchar): PWebKitDOMAttr; cdecl; external;
function webkit_dom_document_create_cdata_section(ADOMDocument: PWebKitDOMDocument; data: Pgchar): PWebKitDOMCDATASection; cdecl; external;
function webkit_dom_document_create_comment(ADOMDocument: PWebKitDOMDocument; data: Pgchar): PWebKitDOMComment; cdecl; external;
function webkit_dom_document_create_css_style_declaration(ADOMDocument: PWebKitDOMDocument): PWebKitDOMCSSStyleDeclaration; cdecl; external;
function webkit_dom_document_create_document_fragment(ADOMDocument: PWebKitDOMDocument): PWebKitDOMDocumentFragment; cdecl; external;
function webkit_dom_document_create_element(ADOMDocument: PWebKitDOMDocument; tag_name: Pgchar): PWebKitDOMElement; cdecl; external;
function webkit_dom_document_create_element_ns(ADOMDocument: PWebKitDOMDocument; namespace_uri: Pgchar; qualified_name: Pgchar): PWebKitDOMElement; cdecl; external;
function webkit_dom_document_create_entity_reference(ADOMDocument: PWebKitDOMDocument; name: Pgchar): PWebKitDOMEntityReference; cdecl; external;
function webkit_dom_document_create_event(ADOMDocument: PWebKitDOMDocument; event_type: Pgchar): PWebKitDOMEvent; cdecl; external;
function webkit_dom_document_create_expression(ADOMDocument: PWebKitDOMDocument; expression: Pgchar; resolver: PWebKitDOMXPathNSResolver): PWebKitDOMXPathExpression; cdecl; external;
function webkit_dom_document_create_node_iterator(ADOMDocument: PWebKitDOMDocument; root: PWebKitDOMNode; what_to_show: gulong; filter: PWebKitDOMNodeFilter; expand_entity_references: gboolean): PWebKitDOMNodeIterator; cdecl; external;
function webkit_dom_document_create_ns_resolver(ADOMDocument: PWebKitDOMDocument; node_resolver: PWebKitDOMNode): PWebKitDOMXPathNSResolver; cdecl; external;
function webkit_dom_document_create_processing_instruction(ADOMDocument: PWebKitDOMDocument; target: Pgchar; data: Pgchar): PWebKitDOMProcessingInstruction; cdecl; external;
function webkit_dom_document_create_range(ADOMDocument: PWebKitDOMDocument): PWebKitDOMRange; cdecl; external;
function webkit_dom_document_create_text_node(ADOMDocument: PWebKitDOMDocument; data: Pgchar): PWebKitDOMText; cdecl; external;
function webkit_dom_document_create_tree_walker(ADOMDocument: PWebKitDOMDocument; root: PWebKitDOMNode; what_to_show: gulong; filter: PWebKitDOMNodeFilter; expand_entity_references: gboolean): PWebKitDOMTreeWalker; cdecl; external;
function webkit_dom_document_element_from_point(ADOMDocument: PWebKitDOMDocument; x: glong; y: glong): PWebKitDOMElement; cdecl; external;
function webkit_dom_document_evaluate(ADOMDocument: PWebKitDOMDocument; expression: Pgchar; context_node: PWebKitDOMNode; resolver: PWebKitDOMXPathNSResolver; type_: gushort; in_result: PWebKitDOMXPathResult): PWebKitDOMXPathResult; cdecl; external;
function webkit_dom_document_exec_command(ADOMDocument: PWebKitDOMDocument; command: Pgchar; user_interface: gboolean; value: Pgchar): gboolean; cdecl; external;
function webkit_dom_document_fragment_get_type: TGType; cdecl; external;
function webkit_dom_document_fragment_query_selector(ADOMDocumentFragment: PWebKitDOMDocumentFragment; selectors: Pgchar): PWebKitDOMElement; cdecl; external;
function webkit_dom_document_fragment_query_selector_all(ADOMDocumentFragment: PWebKitDOMDocumentFragment; selectors: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_document_get_anchors(ADOMDocument: PWebKitDOMDocument): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_document_get_applets(ADOMDocument: PWebKitDOMDocument): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_document_get_body(ADOMDocument: PWebKitDOMDocument): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_document_get_character_set(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_charset(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_compat_mode(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_cookie(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_default_charset(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_default_view(ADOMDocument: PWebKitDOMDocument): PWebKitDOMDOMWindow; cdecl; external;
function webkit_dom_document_get_doctype(ADOMDocument: PWebKitDOMDocument): PWebKitDOMDocumentType; cdecl; external;
function webkit_dom_document_get_document_element(ADOMDocument: PWebKitDOMDocument): PWebKitDOMElement; cdecl; external;
function webkit_dom_document_get_document_uri(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_domain(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_element_by_id(ADOMDocument: PWebKitDOMDocument; element_id: Pgchar): PWebKitDOMElement; cdecl; external;
function webkit_dom_document_get_elements_by_class_name(ADOMDocument: PWebKitDOMDocument; tagname: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_document_get_elements_by_name(ADOMDocument: PWebKitDOMDocument; element_name: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_document_get_elements_by_tag_name(ADOMDocument: PWebKitDOMDocument; tagname: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_document_get_elements_by_tag_name_ns(ADOMDocument: PWebKitDOMDocument; namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_document_get_forms(ADOMDocument: PWebKitDOMDocument): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_document_get_head(ADOMDocument: PWebKitDOMDocument): PWebKitDOMHTMLHeadElement; cdecl; external;
function webkit_dom_document_get_images(ADOMDocument: PWebKitDOMDocument): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_document_get_implementation(ADOMDocument: PWebKitDOMDocument): PWebKitDOMDOMImplementation; cdecl; external;
function webkit_dom_document_get_input_encoding(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_last_modified(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_links(ADOMDocument: PWebKitDOMDocument): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_document_get_override_style(ADOMDocument: PWebKitDOMDocument; element: PWebKitDOMElement; pseudo_element: Pgchar): PWebKitDOMCSSStyleDeclaration; cdecl; external;
function webkit_dom_document_get_preferred_stylesheet_set(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_ready_state(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_referrer(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_selected_stylesheet_set(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_style_sheets(ADOMDocument: PWebKitDOMDocument): PWebKitDOMStyleSheetList; cdecl; external;
function webkit_dom_document_get_title(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_type: TGType; cdecl; external;
function webkit_dom_document_get_webkit_current_full_screen_element(ADOMDocument: PWebKitDOMDocument): PWebKitDOMElement; cdecl; external;
function webkit_dom_document_get_webkit_full_screen_keyboard_input_allowed(ADOMDocument: PWebKitDOMDocument): gboolean; cdecl; external;
function webkit_dom_document_get_webkit_is_full_screen(ADOMDocument: PWebKitDOMDocument): gboolean; cdecl; external;
function webkit_dom_document_get_xml_encoding(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_get_xml_standalone(ADOMDocument: PWebKitDOMDocument): gboolean; cdecl; external;
function webkit_dom_document_get_xml_version(ADOMDocument: PWebKitDOMDocument): Pgchar; cdecl; external;
function webkit_dom_document_import_node(ADOMDocument: PWebKitDOMDocument; imported_node: PWebKitDOMNode; deep: gboolean): PWebKitDOMNode; cdecl; external;
function webkit_dom_document_query_command_enabled(ADOMDocument: PWebKitDOMDocument; command: Pgchar): gboolean; cdecl; external;
function webkit_dom_document_query_command_indeterm(ADOMDocument: PWebKitDOMDocument; command: Pgchar): gboolean; cdecl; external;
function webkit_dom_document_query_command_state(ADOMDocument: PWebKitDOMDocument; command: Pgchar): gboolean; cdecl; external;
function webkit_dom_document_query_command_supported(ADOMDocument: PWebKitDOMDocument; command: Pgchar): gboolean; cdecl; external;
function webkit_dom_document_query_command_value(ADOMDocument: PWebKitDOMDocument; command: Pgchar): Pgchar; cdecl; external;
function webkit_dom_document_query_selector(ADOMDocument: PWebKitDOMDocument; selectors: Pgchar): PWebKitDOMElement; cdecl; external;
function webkit_dom_document_query_selector_all(ADOMDocument: PWebKitDOMDocument; selectors: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_document_type_get_entities(ADOMDocumentType: PWebKitDOMDocumentType): PWebKitDOMNamedNodeMap; cdecl; external;
function webkit_dom_document_type_get_internal_subset(ADOMDocumentType: PWebKitDOMDocumentType): Pgchar; cdecl; external;
function webkit_dom_document_type_get_name(ADOMDocumentType: PWebKitDOMDocumentType): Pgchar; cdecl; external;
function webkit_dom_document_type_get_notations(ADOMDocumentType: PWebKitDOMDocumentType): PWebKitDOMNamedNodeMap; cdecl; external;
function webkit_dom_document_type_get_public_id(ADOMDocumentType: PWebKitDOMDocumentType): Pgchar; cdecl; external;
function webkit_dom_document_type_get_system_id(ADOMDocumentType: PWebKitDOMDocumentType): Pgchar; cdecl; external;
function webkit_dom_document_type_get_type: TGType; cdecl; external;
function webkit_dom_dom_application_cache_dispatch_event(ADOMDOMApplicationCache: PWebKitDOMDOMApplicationCache; evt: PWebKitDOMEvent): gboolean; cdecl; external;
function webkit_dom_dom_application_cache_get_status(ADOMDOMApplicationCache: PWebKitDOMDOMApplicationCache): gushort; cdecl; external;
function webkit_dom_dom_application_cache_get_type: TGType; cdecl; external;
function webkit_dom_dom_implementation_create_css_style_sheet(ADOMDOMImplementation: PWebKitDOMDOMImplementation; title: Pgchar; media: Pgchar): PWebKitDOMCSSStyleSheet; cdecl; external;
function webkit_dom_dom_implementation_create_document(ADOMDOMImplementation: PWebKitDOMDOMImplementation; namespace_uri: Pgchar; qualified_name: Pgchar; doctype: PWebKitDOMDocumentType): PWebKitDOMDocument; cdecl; external;
function webkit_dom_dom_implementation_create_document_type(ADOMDOMImplementation: PWebKitDOMDOMImplementation; qualified_name: Pgchar; public_id: Pgchar; system_id: Pgchar): PWebKitDOMDocumentType; cdecl; external;
function webkit_dom_dom_implementation_create_html_document(ADOMDOMImplementation: PWebKitDOMDOMImplementation; title: Pgchar): PWebKitDOMHTMLDocument; cdecl; external;
function webkit_dom_dom_implementation_get_type: TGType; cdecl; external;
function webkit_dom_dom_implementation_has_feature(ADOMDOMImplementation: PWebKitDOMDOMImplementation; feature: Pgchar; version: Pgchar): gboolean; cdecl; external;
function webkit_dom_dom_mime_type_array_get_length(ADOMDOMMimeTypeArray: PWebKitDOMDOMMimeTypeArray): gulong; cdecl; external;
function webkit_dom_dom_mime_type_array_get_type: TGType; cdecl; external;
function webkit_dom_dom_mime_type_array_item(ADOMDOMMimeTypeArray: PWebKitDOMDOMMimeTypeArray; index: gulong): PWebKitDOMDOMMimeType; cdecl; external;
function webkit_dom_dom_mime_type_array_named_item(ADOMDOMMimeTypeArray: PWebKitDOMDOMMimeTypeArray; name: Pgchar): PWebKitDOMDOMMimeType; cdecl; external;
function webkit_dom_dom_mime_type_get_description(ADOMDOMMimeType: PWebKitDOMDOMMimeType): Pgchar; cdecl; external;
function webkit_dom_dom_mime_type_get_enabled_plugin(ADOMDOMMimeType: PWebKitDOMDOMMimeType): PWebKitDOMDOMPlugin; cdecl; external;
function webkit_dom_dom_mime_type_get_suffixes(ADOMDOMMimeType: PWebKitDOMDOMMimeType): Pgchar; cdecl; external;
function webkit_dom_dom_mime_type_get_type: TGType; cdecl; external;
function webkit_dom_dom_plugin_array_get_length(ADOMDOMPluginArray: PWebKitDOMDOMPluginArray): gulong; cdecl; external;
function webkit_dom_dom_plugin_array_get_type: TGType; cdecl; external;
function webkit_dom_dom_plugin_array_item(ADOMDOMPluginArray: PWebKitDOMDOMPluginArray; index: gulong): PWebKitDOMDOMPlugin; cdecl; external;
function webkit_dom_dom_plugin_array_named_item(ADOMDOMPluginArray: PWebKitDOMDOMPluginArray; name: Pgchar): PWebKitDOMDOMPlugin; cdecl; external;
function webkit_dom_dom_plugin_get_description(ADOMDOMPlugin: PWebKitDOMDOMPlugin): Pgchar; cdecl; external;
function webkit_dom_dom_plugin_get_filename(ADOMDOMPlugin: PWebKitDOMDOMPlugin): Pgchar; cdecl; external;
function webkit_dom_dom_plugin_get_length(ADOMDOMPlugin: PWebKitDOMDOMPlugin): gulong; cdecl; external;
function webkit_dom_dom_plugin_get_name(ADOMDOMPlugin: PWebKitDOMDOMPlugin): Pgchar; cdecl; external;
function webkit_dom_dom_plugin_get_type: TGType; cdecl; external;
function webkit_dom_dom_plugin_item(ADOMDOMPlugin: PWebKitDOMDOMPlugin; index: gulong): PWebKitDOMDOMMimeType; cdecl; external;
function webkit_dom_dom_plugin_named_item(ADOMDOMPlugin: PWebKitDOMDOMPlugin; name: Pgchar): PWebKitDOMDOMMimeType; cdecl; external;
function webkit_dom_dom_selection_contains_node(ADOMDOMSelection: PWebKitDOMDOMSelection; node: PWebKitDOMNode; allow_partial: gboolean): gboolean; cdecl; external;
function webkit_dom_dom_selection_get_anchor_node(ADOMDOMSelection: PWebKitDOMDOMSelection): PWebKitDOMNode; cdecl; external;
function webkit_dom_dom_selection_get_anchor_offset(ADOMDOMSelection: PWebKitDOMDOMSelection): glong; cdecl; external;
function webkit_dom_dom_selection_get_base_node(ADOMDOMSelection: PWebKitDOMDOMSelection): PWebKitDOMNode; cdecl; external;
function webkit_dom_dom_selection_get_base_offset(ADOMDOMSelection: PWebKitDOMDOMSelection): glong; cdecl; external;
function webkit_dom_dom_selection_get_extent_node(ADOMDOMSelection: PWebKitDOMDOMSelection): PWebKitDOMNode; cdecl; external;
function webkit_dom_dom_selection_get_extent_offset(ADOMDOMSelection: PWebKitDOMDOMSelection): glong; cdecl; external;
function webkit_dom_dom_selection_get_focus_node(ADOMDOMSelection: PWebKitDOMDOMSelection): PWebKitDOMNode; cdecl; external;
function webkit_dom_dom_selection_get_focus_offset(ADOMDOMSelection: PWebKitDOMDOMSelection): glong; cdecl; external;
function webkit_dom_dom_selection_get_is_collapsed(ADOMDOMSelection: PWebKitDOMDOMSelection): gboolean; cdecl; external;
function webkit_dom_dom_selection_get_range_at(ADOMDOMSelection: PWebKitDOMDOMSelection; index: glong): PWebKitDOMRange; cdecl; external;
function webkit_dom_dom_selection_get_range_count(ADOMDOMSelection: PWebKitDOMDOMSelection): glong; cdecl; external;
function webkit_dom_dom_selection_get_type: TGType; cdecl; external;
function webkit_dom_dom_settable_token_list_get_type: TGType; cdecl; external;
function webkit_dom_dom_settable_token_list_get_value(ADOMDOMSettableTokenList: PWebKitDOMDOMSettableTokenList): Pgchar; cdecl; external;
function webkit_dom_dom_string_list_contains(ADOMDOMStringList: PWebKitDOMDOMStringList; string_: Pgchar): gboolean; cdecl; external;
function webkit_dom_dom_string_list_get_length(ADOMDOMStringList: PWebKitDOMDOMStringList): gulong; cdecl; external;
function webkit_dom_dom_string_list_get_type: TGType; cdecl; external;
function webkit_dom_dom_string_list_item(ADOMDOMStringList: PWebKitDOMDOMStringList; index: gulong): Pgchar; cdecl; external;
function webkit_dom_dom_string_map_get_type: TGType; cdecl; external;
function webkit_dom_dom_token_list_contains(ADOMDOMTokenList: PWebKitDOMDOMTokenList; token: Pgchar): gboolean; cdecl; external;
function webkit_dom_dom_token_list_get_length(ADOMDOMTokenList: PWebKitDOMDOMTokenList): gulong; cdecl; external;
function webkit_dom_dom_token_list_get_type: TGType; cdecl; external;
function webkit_dom_dom_token_list_item(ADOMDOMTokenList: PWebKitDOMDOMTokenList; index: gulong): Pgchar; cdecl; external;
function webkit_dom_dom_token_list_toggle(ADOMDOMTokenList: PWebKitDOMDOMTokenList; token: Pgchar): gboolean; cdecl; external;
function webkit_dom_dom_window_atob(ADOMDOMWindow: PWebKitDOMDOMWindow; string_: Pgchar): Pgchar; cdecl; external;
function webkit_dom_dom_window_btoa(ADOMDOMWindow: PWebKitDOMDOMWindow; string_: Pgchar): Pgchar; cdecl; external;
function webkit_dom_dom_window_confirm(ADOMDOMWindow: PWebKitDOMDOMWindow; message: Pgchar): gboolean; cdecl; external;
function webkit_dom_dom_window_dispatch_event(ADOMDOMWindow: PWebKitDOMDOMWindow; evt: PWebKitDOMEvent): gboolean; cdecl; external;
function webkit_dom_dom_window_find(ADOMDOMWindow: PWebKitDOMDOMWindow; string_: Pgchar; case_sensitive: gboolean; backwards: gboolean; wrap: gboolean; whole_word: gboolean; search_in_frames: gboolean; show_dialog: gboolean): gboolean; cdecl; external;
function webkit_dom_dom_window_get_application_cache(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMDOMApplicationCache; cdecl; external;
function webkit_dom_dom_window_get_closed(ADOMDOMWindow: PWebKitDOMDOMWindow): gboolean; cdecl; external;
function webkit_dom_dom_window_get_computed_style(ADOMDOMWindow: PWebKitDOMDOMWindow; element: PWebKitDOMElement; pseudo_element: Pgchar): PWebKitDOMCSSStyleDeclaration; cdecl; external;
function webkit_dom_dom_window_get_default_status(ADOMDOMWindow: PWebKitDOMDOMWindow): Pgchar; cdecl; external;
function webkit_dom_dom_window_get_document(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMDocument; cdecl; external;
function webkit_dom_dom_window_get_frame_element(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMElement; cdecl; external;
function webkit_dom_dom_window_get_history(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMHistory; cdecl; external;
function webkit_dom_dom_window_get_local_storage(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMStorage; cdecl; external;
function webkit_dom_dom_window_get_name(ADOMDOMWindow: PWebKitDOMDOMWindow): Pgchar; cdecl; external;
function webkit_dom_dom_window_get_page_x_offset(ADOMDOMWindow: PWebKitDOMDOMWindow): glong; cdecl; external;
function webkit_dom_dom_window_get_page_y_offset(ADOMDOMWindow: PWebKitDOMDOMWindow): glong; cdecl; external;
function webkit_dom_dom_window_get_selection(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMDOMSelection; cdecl; external;
function webkit_dom_dom_window_get_session_storage(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMStorage; cdecl; external;
function webkit_dom_dom_window_get_status(ADOMDOMWindow: PWebKitDOMDOMWindow): Pgchar; cdecl; external;
function webkit_dom_dom_window_get_style_media(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMStyleMedia; cdecl; external;
function webkit_dom_dom_window_get_type: TGType; cdecl; external;
function webkit_dom_dom_window_get_window(ADOMDOMWindow: PWebKitDOMDOMWindow): PWebKitDOMDOMWindow; cdecl; external;
function webkit_dom_dom_window_match_media(ADOMDOMWindow: PWebKitDOMDOMWindow; query: Pgchar): PWebKitDOMMediaQueryList; cdecl; external;
function webkit_dom_dom_window_prompt(ADOMDOMWindow: PWebKitDOMDOMWindow; message: Pgchar; default_value: Pgchar): Pgchar; cdecl; external;
function webkit_dom_dom_window_webkit_convert_point_from_node_to_page(ADOMDOMWindow: PWebKitDOMDOMWindow; node: PWebKitDOMNode; p: PWebKitDOMWebKitPoint): PWebKitDOMWebKitPoint; cdecl; external;
function webkit_dom_dom_window_webkit_convert_point_from_page_to_node(ADOMDOMWindow: PWebKitDOMDOMWindow; node: PWebKitDOMNode; p: PWebKitDOMWebKitPoint): PWebKitDOMWebKitPoint; cdecl; external;
function webkit_dom_element_contains(ADOMElement: PWebKitDOMElement; element: PWebKitDOMElement): gboolean; cdecl; external;
function webkit_dom_element_get_attribute(ADOMElement: PWebKitDOMElement; name: Pgchar): Pgchar; cdecl; external;
function webkit_dom_element_get_attribute_node(ADOMElement: PWebKitDOMElement; name: Pgchar): PWebKitDOMAttr; cdecl; external;
function webkit_dom_element_get_attribute_node_ns(ADOMElement: PWebKitDOMElement; namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMAttr; cdecl; external;
function webkit_dom_element_get_attribute_ns(ADOMElement: PWebKitDOMElement; namespace_uri: Pgchar; local_name: Pgchar): Pgchar; cdecl; external;
function webkit_dom_element_get_child_element_count(ADOMElement: PWebKitDOMElement): gulong; cdecl; external;
function webkit_dom_element_get_client_height(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_client_left(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_client_top(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_client_width(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_elements_by_class_name(ADOMElement: PWebKitDOMElement; name: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_element_get_elements_by_tag_name(ADOMElement: PWebKitDOMElement; name: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_element_get_elements_by_tag_name_ns(ADOMElement: PWebKitDOMElement; namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_element_get_first_element_child(ADOMElement: PWebKitDOMElement): PWebKitDOMElement; cdecl; external;
function webkit_dom_element_get_last_element_child(ADOMElement: PWebKitDOMElement): PWebKitDOMElement; cdecl; external;
function webkit_dom_element_get_next_element_sibling(ADOMElement: PWebKitDOMElement): PWebKitDOMElement; cdecl; external;
function webkit_dom_element_get_offset_height(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_offset_left(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_offset_parent(ADOMElement: PWebKitDOMElement): PWebKitDOMElement; cdecl; external;
function webkit_dom_element_get_offset_top(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_offset_width(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_previous_element_sibling(ADOMElement: PWebKitDOMElement): PWebKitDOMElement; cdecl; external;
function webkit_dom_element_get_scroll_height(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_scroll_left(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_scroll_top(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_scroll_width(ADOMElement: PWebKitDOMElement): glong; cdecl; external;
function webkit_dom_element_get_style(ADOMElement: PWebKitDOMElement): PWebKitDOMCSSStyleDeclaration; cdecl; external;
function webkit_dom_element_get_tag_name(ADOMElement: PWebKitDOMElement): Pgchar; cdecl; external;
function webkit_dom_element_get_type: TGType; cdecl; external;
function webkit_dom_element_has_attribute(ADOMElement: PWebKitDOMElement; name: Pgchar): gboolean; cdecl; external;
function webkit_dom_element_has_attribute_ns(ADOMElement: PWebKitDOMElement; namespace_uri: Pgchar; local_name: Pgchar): gboolean; cdecl; external;
function webkit_dom_element_query_selector(ADOMElement: PWebKitDOMElement; selectors: Pgchar): PWebKitDOMElement; cdecl; external;
function webkit_dom_element_query_selector_all(ADOMElement: PWebKitDOMElement; selectors: Pgchar): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_element_remove_attribute_node(ADOMElement: PWebKitDOMElement; old_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl; external;
function webkit_dom_element_set_attribute_node(ADOMElement: PWebKitDOMElement; new_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl; external;
function webkit_dom_element_set_attribute_node_ns(ADOMElement: PWebKitDOMElement; new_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl; external;
function webkit_dom_element_webkit_get_animations(ADOMElement: PWebKitDOMElement): PWebKitDOMWebKitAnimationList; cdecl; external;
function webkit_dom_element_webkit_matches_selector(ADOMElement: PWebKitDOMElement; selectors: Pgchar): gboolean; cdecl; external;
function webkit_dom_entity_reference_get_type: TGType; cdecl; external;
function webkit_dom_event_get_bubbles(ADOMEvent: PWebKitDOMEvent): gboolean; cdecl; external;
function webkit_dom_event_get_cancel_bubble(ADOMEvent: PWebKitDOMEvent): gboolean; cdecl; external;
function webkit_dom_event_get_cancelable(ADOMEvent: PWebKitDOMEvent): gboolean; cdecl; external;
function webkit_dom_event_get_current_target(ADOMEvent: PWebKitDOMEvent): PWebKitDOMEventTarget; cdecl; external;
function webkit_dom_event_get_default_prevented(ADOMEvent: PWebKitDOMEvent): gboolean; cdecl; external;
function webkit_dom_event_get_event_phase(ADOMEvent: PWebKitDOMEvent): gushort; cdecl; external;
function webkit_dom_event_get_return_value(ADOMEvent: PWebKitDOMEvent): gboolean; cdecl; external;
function webkit_dom_event_get_src_element(ADOMEvent: PWebKitDOMEvent): PWebKitDOMEventTarget; cdecl; external;
function webkit_dom_event_get_target(ADOMEvent: PWebKitDOMEvent): PWebKitDOMEventTarget; cdecl; external;
function webkit_dom_event_get_time_stamp(ADOMEvent: PWebKitDOMEvent): guint32; cdecl; external;
function webkit_dom_event_get_type: TGType; cdecl; external;
function webkit_dom_event_target_add_event_listener(ADOMEventTarget: PWebKitDOMEventTarget; eventName: Pgchar; handler: TGCallback; bubble: gboolean; userData: gpointer): gboolean; cdecl; external;
function webkit_dom_event_target_get_type: TGType; cdecl; external;
function webkit_dom_event_target_remove_event_listener(ADOMEventTarget: PWebKitDOMEventTarget; eventName: Pgchar; handler: TGCallback; bubble: gboolean): gboolean; cdecl; external;
function webkit_dom_file_get_file_name(ADOMFile: PWebKitDOMFile): Pgchar; cdecl; external;
function webkit_dom_file_get_file_size(ADOMFile: PWebKitDOMFile): guint64; cdecl; external;
function webkit_dom_file_get_name(ADOMFile: PWebKitDOMFile): Pgchar; cdecl; external;
function webkit_dom_file_get_type: TGType; cdecl; external;
function webkit_dom_file_list_get_length(ADOMFileList: PWebKitDOMFileList): gulong; cdecl; external;
function webkit_dom_file_list_get_type: TGType; cdecl; external;
function webkit_dom_file_list_item(ADOMFileList: PWebKitDOMFileList; index: gulong): PWebKitDOMFile; cdecl; external;
function webkit_dom_history_get_length(ADOMHistory: PWebKitDOMHistory): gulong; cdecl; external;
function webkit_dom_history_get_type: TGType; cdecl; external;
function webkit_dom_html_anchor_element_get_access_key(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_charset(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_coords(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_hash(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_host(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_hostname(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_href(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_hreflang(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_name(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_origin(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_parameter(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; name: Pgchar): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_pathname(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_port(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_protocol(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_rel(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_rev(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_search(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_shape(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_target(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_text(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement): Pgchar; cdecl; external;
function webkit_dom_html_anchor_element_get_type: TGType; cdecl; external;
function webkit_dom_html_applet_element_get_align(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_applet_element_get_alt(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_applet_element_get_archive(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_applet_element_get_code(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_applet_element_get_code_base(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_applet_element_get_height(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_applet_element_get_hspace(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): glong; cdecl; external;
function webkit_dom_html_applet_element_get_name(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_applet_element_get_object(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_applet_element_get_type: TGType; cdecl; external;
function webkit_dom_html_applet_element_get_vspace(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): glong; cdecl; external;
function webkit_dom_html_applet_element_get_width(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_access_key(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_alt(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_coords(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_hash(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_host(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_hostname(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_href(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_no_href(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): gboolean; cdecl; external;
function webkit_dom_html_area_element_get_pathname(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_port(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_protocol(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_search(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_shape(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_target(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_area_element_get_type: TGType; cdecl; external;
function webkit_dom_html_audio_element_get_type: TGType; cdecl; external;
function webkit_dom_html_base_element_get_href(ADOMHTMLBaseElement: PWebKitDOMHTMLBaseElement): Pgchar; cdecl; external;
function webkit_dom_html_base_element_get_target(ADOMHTMLBaseElement: PWebKitDOMHTMLBaseElement): Pgchar; cdecl; external;
function webkit_dom_html_base_element_get_type: TGType; cdecl; external;
function webkit_dom_html_base_font_element_get_color(ADOMHTMLBaseFontElement: PWebKitDOMHTMLBaseFontElement): Pgchar; cdecl; external;
function webkit_dom_html_base_font_element_get_face(ADOMHTMLBaseFontElement: PWebKitDOMHTMLBaseFontElement): Pgchar; cdecl; external;
function webkit_dom_html_base_font_element_get_size(ADOMHTMLBaseFontElement: PWebKitDOMHTMLBaseFontElement): glong; cdecl; external;
function webkit_dom_html_base_font_element_get_type: TGType; cdecl; external;
function webkit_dom_html_blockquote_element_get_cite(ADOMHTMLBlockquoteElement: PWebKitDOMHTMLBlockquoteElement): Pgchar; cdecl; external;
function webkit_dom_html_blockquote_element_get_type: TGType; cdecl; external;
function webkit_dom_html_body_element_get_a_link(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement): Pgchar; cdecl; external;
function webkit_dom_html_body_element_get_background(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement): Pgchar; cdecl; external;
function webkit_dom_html_body_element_get_bg_color(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement): Pgchar; cdecl; external;
function webkit_dom_html_body_element_get_link(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement): Pgchar; cdecl; external;
function webkit_dom_html_body_element_get_text(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement): Pgchar; cdecl; external;
function webkit_dom_html_body_element_get_type: TGType; cdecl; external;
function webkit_dom_html_body_element_get_v_link(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_check_validity(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): gboolean; cdecl; external;
function webkit_dom_html_button_element_get_access_key(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_get_autofocus(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): gboolean; cdecl; external;
function webkit_dom_html_button_element_get_disabled(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): gboolean; cdecl; external;
function webkit_dom_html_button_element_get_form(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_button_element_get_form_action(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_get_form_enctype(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_get_form_method(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_get_form_no_validate(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): gboolean; cdecl; external;
function webkit_dom_html_button_element_get_form_target(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_get_labels(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_html_button_element_get_name(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_get_type: TGType; cdecl; external;
function webkit_dom_html_button_element_get_validation_message(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_get_validity(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): PWebKitDOMValidityState; cdecl; external;
function webkit_dom_html_button_element_get_value(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): Pgchar; cdecl; external;
function webkit_dom_html_button_element_get_will_validate(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement): gboolean; cdecl; external;
function webkit_dom_html_canvas_element_get_height(ADOMHTMLCanvasElement: PWebKitDOMHTMLCanvasElement): glong; cdecl; external;
function webkit_dom_html_canvas_element_get_type: TGType; cdecl; external;
function webkit_dom_html_canvas_element_get_width(ADOMHTMLCanvasElement: PWebKitDOMHTMLCanvasElement): glong; cdecl; external;
function webkit_dom_html_collection_get_length(ADOMHTMLCollection: PWebKitDOMHTMLCollection): gulong; cdecl; external;
function webkit_dom_html_collection_get_type: TGType; cdecl; external;
function webkit_dom_html_collection_item(ADOMHTMLCollection: PWebKitDOMHTMLCollection; index: gulong): PWebKitDOMNode; cdecl; external;
function webkit_dom_html_collection_named_item(ADOMHTMLCollection: PWebKitDOMHTMLCollection; name: Pgchar): PWebKitDOMNode; cdecl; external;
function webkit_dom_html_details_element_get_open(ADOMHTMLDetailsElement: PWebKitDOMHTMLDetailsElement): gboolean; cdecl; external;
function webkit_dom_html_details_element_get_type: TGType; cdecl; external;
function webkit_dom_html_directory_element_get_compact(ADOMHTMLDirectoryElement: PWebKitDOMHTMLDirectoryElement): gboolean; cdecl; external;
function webkit_dom_html_directory_element_get_type: TGType; cdecl; external;
function webkit_dom_html_div_element_get_align(ADOMHTMLDivElement: PWebKitDOMHTMLDivElement): Pgchar; cdecl; external;
function webkit_dom_html_div_element_get_type: TGType; cdecl; external;
function webkit_dom_html_document_get_active_element(ADOMHTMLDocument: PWebKitDOMHTMLDocument): PWebKitDOMElement; cdecl; external;
function webkit_dom_html_document_get_alink_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument): Pgchar; cdecl; external;
function webkit_dom_html_document_get_bg_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument): Pgchar; cdecl; external;
function webkit_dom_html_document_get_compat_mode(ADOMHTMLDocument: PWebKitDOMHTMLDocument): Pgchar; cdecl; external;
function webkit_dom_html_document_get_design_mode(ADOMHTMLDocument: PWebKitDOMHTMLDocument): Pgchar; cdecl; external;
function webkit_dom_html_document_get_dir(ADOMHTMLDocument: PWebKitDOMHTMLDocument): Pgchar; cdecl; external;
function webkit_dom_html_document_get_embeds(ADOMHTMLDocument: PWebKitDOMHTMLDocument): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_document_get_fg_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument): Pgchar; cdecl; external;
function webkit_dom_html_document_get_height(ADOMHTMLDocument: PWebKitDOMHTMLDocument): glong; cdecl; external;
function webkit_dom_html_document_get_link_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument): Pgchar; cdecl; external;
function webkit_dom_html_document_get_plugins(ADOMHTMLDocument: PWebKitDOMHTMLDocument): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_document_get_scripts(ADOMHTMLDocument: PWebKitDOMHTMLDocument): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_document_get_type: TGType; cdecl; external;
function webkit_dom_html_document_get_vlink_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument): Pgchar; cdecl; external;
function webkit_dom_html_document_get_width(ADOMHTMLDocument: PWebKitDOMHTMLDocument): glong; cdecl; external;
function webkit_dom_html_document_has_focus(ADOMHTMLDocument: PWebKitDOMHTMLDocument): gboolean; cdecl; external;
function webkit_dom_html_element_get_children(ADOMHTMLElement: PWebKitDOMHTMLElement): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_element_get_class_list(ADOMHTMLElement: PWebKitDOMHTMLElement): PWebKitDOMDOMTokenList; cdecl; external;
function webkit_dom_html_element_get_class_name(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_content_editable(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_dir(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_draggable(ADOMHTMLElement: PWebKitDOMHTMLElement): gboolean; cdecl; external;
function webkit_dom_html_element_get_hidden(ADOMHTMLElement: PWebKitDOMHTMLElement): gboolean; cdecl; external;
function webkit_dom_html_element_get_id(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_inner_html(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_inner_text(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_is_content_editable(ADOMHTMLElement: PWebKitDOMHTMLElement): gboolean; cdecl; external;
function webkit_dom_html_element_get_lang(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_outer_html(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_outer_text(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_spellcheck(ADOMHTMLElement: PWebKitDOMHTMLElement): gboolean; cdecl; external;
function webkit_dom_html_element_get_tab_index(ADOMHTMLElement: PWebKitDOMHTMLElement): glong; cdecl; external;
function webkit_dom_html_element_get_title(ADOMHTMLElement: PWebKitDOMHTMLElement): Pgchar; cdecl; external;
function webkit_dom_html_element_get_type: TGType; cdecl; external;
function webkit_dom_html_element_insert_adjacent_element(ADOMHTMLElement: PWebKitDOMHTMLElement; where: Pgchar; element: PWebKitDOMElement): PWebKitDOMElement; cdecl; external;
function webkit_dom_html_embed_element_get_align(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement): Pgchar; cdecl; external;
function webkit_dom_html_embed_element_get_height(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement): glong; cdecl; external;
function webkit_dom_html_embed_element_get_name(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement): Pgchar; cdecl; external;
function webkit_dom_html_embed_element_get_src(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement): Pgchar; cdecl; external;
function webkit_dom_html_embed_element_get_type: TGType; cdecl; external;
function webkit_dom_html_embed_element_get_width(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement): glong; cdecl; external;
function webkit_dom_html_field_set_element_check_validity(ADOMHTMLFieldSetElement: PWebKitDOMHTMLFieldSetElement): gboolean; cdecl; external;
function webkit_dom_html_field_set_element_get_form(ADOMHTMLFieldSetElement: PWebKitDOMHTMLFieldSetElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_field_set_element_get_type: TGType; cdecl; external;
function webkit_dom_html_field_set_element_get_validation_message(ADOMHTMLFieldSetElement: PWebKitDOMHTMLFieldSetElement): Pgchar; cdecl; external;
function webkit_dom_html_field_set_element_get_validity(ADOMHTMLFieldSetElement: PWebKitDOMHTMLFieldSetElement): PWebKitDOMValidityState; cdecl; external;
function webkit_dom_html_field_set_element_get_will_validate(ADOMHTMLFieldSetElement: PWebKitDOMHTMLFieldSetElement): gboolean; cdecl; external;
function webkit_dom_html_font_element_get_color(ADOMHTMLFontElement: PWebKitDOMHTMLFontElement): Pgchar; cdecl; external;
function webkit_dom_html_font_element_get_face(ADOMHTMLFontElement: PWebKitDOMHTMLFontElement): Pgchar; cdecl; external;
function webkit_dom_html_font_element_get_size(ADOMHTMLFontElement: PWebKitDOMHTMLFontElement): Pgchar; cdecl; external;
function webkit_dom_html_font_element_get_type: TGType; cdecl; external;
function webkit_dom_html_form_element_check_validity(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): gboolean; cdecl; external;
function webkit_dom_html_form_element_get_accept_charset(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): Pgchar; cdecl; external;
function webkit_dom_html_form_element_get_action(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): Pgchar; cdecl; external;
function webkit_dom_html_form_element_get_elements(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_form_element_get_encoding(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): Pgchar; cdecl; external;
function webkit_dom_html_form_element_get_enctype(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): Pgchar; cdecl; external;
function webkit_dom_html_form_element_get_length(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): glong; cdecl; external;
function webkit_dom_html_form_element_get_method(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): Pgchar; cdecl; external;
function webkit_dom_html_form_element_get_name(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): Pgchar; cdecl; external;
function webkit_dom_html_form_element_get_no_validate(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): gboolean; cdecl; external;
function webkit_dom_html_form_element_get_target(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement): Pgchar; cdecl; external;
function webkit_dom_html_form_element_get_type: TGType; cdecl; external;
function webkit_dom_html_frame_element_get_content_document(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): PWebKitDOMDocument; cdecl; external;
function webkit_dom_html_frame_element_get_content_window(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): PWebKitDOMDOMWindow; cdecl; external;
function webkit_dom_html_frame_element_get_frame_border(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_element_get_height(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): glong; cdecl; external;
function webkit_dom_html_frame_element_get_long_desc(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_element_get_margin_height(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_element_get_margin_width(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_element_get_name(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_element_get_no_resize(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): gboolean; cdecl; external;
function webkit_dom_html_frame_element_get_scrolling(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_element_get_src(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_element_get_type: TGType; cdecl; external;
function webkit_dom_html_frame_element_get_width(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement): glong; cdecl; external;
function webkit_dom_html_frame_set_element_get_cols(ADOMHTMLFrameSetElement: PWebKitDOMHTMLFrameSetElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_set_element_get_rows(ADOMHTMLFrameSetElement: PWebKitDOMHTMLFrameSetElement): Pgchar; cdecl; external;
function webkit_dom_html_frame_set_element_get_type: TGType; cdecl; external;
function webkit_dom_html_head_element_get_profile(ADOMHTMLHeadElement: PWebKitDOMHTMLHeadElement): Pgchar; cdecl; external;
function webkit_dom_html_head_element_get_type: TGType; cdecl; external;
function webkit_dom_html_heading_element_get_align(ADOMHTMLHeadingElement: PWebKitDOMHTMLHeadingElement): Pgchar; cdecl; external;
function webkit_dom_html_heading_element_get_type: TGType; cdecl; external;
function webkit_dom_html_html_element_get_manifest(ADOMHTMLHtmlElement: PWebKitDOMHTMLHtmlElement): Pgchar; cdecl; external;
function webkit_dom_html_html_element_get_type: TGType; cdecl; external;
function webkit_dom_html_html_element_get_version(ADOMHTMLHtmlElement: PWebKitDOMHTMLHtmlElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_align(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_content_document(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): PWebKitDOMDocument; cdecl; external;
function webkit_dom_html_iframe_element_get_content_window(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): PWebKitDOMDOMWindow; cdecl; external;
function webkit_dom_html_iframe_element_get_frame_border(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_height(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_long_desc(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_margin_height(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_margin_width(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_name(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_sandbox(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_scrolling(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_src(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_iframe_element_get_type: TGType; cdecl; external;
function webkit_dom_html_iframe_element_get_width(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_align(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_alt(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_border(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_complete(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): gboolean; cdecl; external;
function webkit_dom_html_image_element_get_height(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): glong; cdecl; external;
function webkit_dom_html_image_element_get_hspace(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): glong; cdecl; external;
function webkit_dom_html_image_element_get_is_map(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): gboolean; cdecl; external;
function webkit_dom_html_image_element_get_long_desc(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_lowsrc(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_name(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_natural_height(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): glong; cdecl; external;
function webkit_dom_html_image_element_get_natural_width(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): glong; cdecl; external;
function webkit_dom_html_image_element_get_src(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_type: TGType; cdecl; external;
function webkit_dom_html_image_element_get_use_map(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): Pgchar; cdecl; external;
function webkit_dom_html_image_element_get_vspace(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): glong; cdecl; external;
function webkit_dom_html_image_element_get_width(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): glong; cdecl; external;
function webkit_dom_html_image_element_get_x(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): glong; cdecl; external;
function webkit_dom_html_image_element_get_y(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement): glong; cdecl; external;
function webkit_dom_html_input_element_check_validity(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_accept(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_access_key(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_align(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_alt(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_autofocus(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_checked(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_default_checked(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_default_value(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_disabled(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_files(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): PWebKitDOMFileList; cdecl; external;
function webkit_dom_html_input_element_get_form(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_input_element_get_form_action(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_form_enctype(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_form_method(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_form_no_validate(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_form_target(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_incremental(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_indeterminate(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_labels(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_html_input_element_get_list(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_html_input_element_get_max(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_max_length(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): glong; cdecl; external;
function webkit_dom_html_input_element_get_min(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_multiple(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_name(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_pattern(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_placeholder(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_read_only(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_required(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_input_element_get_selected_option(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): PWebKitDOMHTMLOptionElement; cdecl; external;
function webkit_dom_html_input_element_get_size(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gulong; cdecl; external;
function webkit_dom_html_input_element_get_src(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_step(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_type: TGType; cdecl; external;
function webkit_dom_html_input_element_get_use_map(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_validation_message(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_validity(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): PWebKitDOMValidityState; cdecl; external;
function webkit_dom_html_input_element_get_value(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): Pgchar; cdecl; external;
function webkit_dom_html_input_element_get_value_as_number(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gdouble; cdecl; external;
function webkit_dom_html_input_element_get_will_validate(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement): gboolean; cdecl; external;
function webkit_dom_html_is_index_element_get_form(ADOMHTMLIsIndexElement: PWebKitDOMHTMLIsIndexElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_is_index_element_get_prompt(ADOMHTMLIsIndexElement: PWebKitDOMHTMLIsIndexElement): Pgchar; cdecl; external;
function webkit_dom_html_is_index_element_get_type: TGType; cdecl; external;
function webkit_dom_html_keygen_element_check_validity(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): gboolean; cdecl; external;
function webkit_dom_html_keygen_element_get_autofocus(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): gboolean; cdecl; external;
function webkit_dom_html_keygen_element_get_challenge(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): Pgchar; cdecl; external;
function webkit_dom_html_keygen_element_get_disabled(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): gboolean; cdecl; external;
function webkit_dom_html_keygen_element_get_form(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_keygen_element_get_keytype(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): Pgchar; cdecl; external;
function webkit_dom_html_keygen_element_get_labels(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_html_keygen_element_get_name(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): Pgchar; cdecl; external;
function webkit_dom_html_keygen_element_get_type: TGType; cdecl; external;
function webkit_dom_html_keygen_element_get_validation_message(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): Pgchar; cdecl; external;
function webkit_dom_html_keygen_element_get_validity(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): PWebKitDOMValidityState; cdecl; external;
function webkit_dom_html_keygen_element_get_will_validate(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement): gboolean; cdecl; external;
function webkit_dom_html_label_element_get_access_key(ADOMHTMLLabelElement: PWebKitDOMHTMLLabelElement): Pgchar; cdecl; external;
function webkit_dom_html_label_element_get_control(ADOMHTMLLabelElement: PWebKitDOMHTMLLabelElement): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_html_label_element_get_form(ADOMHTMLLabelElement: PWebKitDOMHTMLLabelElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_label_element_get_html_for(ADOMHTMLLabelElement: PWebKitDOMHTMLLabelElement): Pgchar; cdecl; external;
function webkit_dom_html_label_element_get_type: TGType; cdecl; external;
function webkit_dom_html_legend_element_get_access_key(ADOMHTMLLegendElement: PWebKitDOMHTMLLegendElement): Pgchar; cdecl; external;
function webkit_dom_html_legend_element_get_align(ADOMHTMLLegendElement: PWebKitDOMHTMLLegendElement): Pgchar; cdecl; external;
function webkit_dom_html_legend_element_get_form(ADOMHTMLLegendElement: PWebKitDOMHTMLLegendElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_legend_element_get_type: TGType; cdecl; external;
function webkit_dom_html_link_element_get_charset(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): Pgchar; cdecl; external;
function webkit_dom_html_link_element_get_disabled(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): gboolean; cdecl; external;
function webkit_dom_html_link_element_get_href(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): Pgchar; cdecl; external;
function webkit_dom_html_link_element_get_hreflang(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): Pgchar; cdecl; external;
function webkit_dom_html_link_element_get_media(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): Pgchar; cdecl; external;
function webkit_dom_html_link_element_get_rel(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): Pgchar; cdecl; external;
function webkit_dom_html_link_element_get_rev(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): Pgchar; cdecl; external;
function webkit_dom_html_link_element_get_sheet(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): PWebKitDOMStyleSheet; cdecl; external;
function webkit_dom_html_link_element_get_target(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement): Pgchar; cdecl; external;
function webkit_dom_html_link_element_get_type: TGType; cdecl; external;
function webkit_dom_html_map_element_get_areas(ADOMHTMLMapElement: PWebKitDOMHTMLMapElement): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_map_element_get_name(ADOMHTMLMapElement: PWebKitDOMHTMLMapElement): Pgchar; cdecl; external;
function webkit_dom_html_map_element_get_type: TGType; cdecl; external;
function webkit_dom_html_marquee_element_get_behavior(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): Pgchar; cdecl; external;
function webkit_dom_html_marquee_element_get_bg_color(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): Pgchar; cdecl; external;
function webkit_dom_html_marquee_element_get_direction(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): Pgchar; cdecl; external;
function webkit_dom_html_marquee_element_get_height(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): Pgchar; cdecl; external;
function webkit_dom_html_marquee_element_get_hspace(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): gulong; cdecl; external;
function webkit_dom_html_marquee_element_get_loop(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): glong; cdecl; external;
function webkit_dom_html_marquee_element_get_scroll_amount(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): glong; cdecl; external;
function webkit_dom_html_marquee_element_get_scroll_delay(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): glong; cdecl; external;
function webkit_dom_html_marquee_element_get_true_speed(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): gboolean; cdecl; external;
function webkit_dom_html_marquee_element_get_type: TGType; cdecl; external;
function webkit_dom_html_marquee_element_get_vspace(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): gulong; cdecl; external;
function webkit_dom_html_marquee_element_get_width(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement): Pgchar; cdecl; external;
function webkit_dom_html_media_element_can_play_type(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; type_: Pgchar): Pgchar; cdecl; external;
function webkit_dom_html_media_element_get_autoplay(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_buffered(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): PWebKitDOMTimeRanges; cdecl; external;
function webkit_dom_html_media_element_get_controls(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_current_src(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): Pgchar; cdecl; external;
function webkit_dom_html_media_element_get_current_time(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gfloat; cdecl; external;
function webkit_dom_html_media_element_get_default_playback_rate(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gfloat; cdecl; external;
function webkit_dom_html_media_element_get_duration(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gfloat; cdecl; external;
function webkit_dom_html_media_element_get_ended(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_error(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): PWebKitDOMMediaError; cdecl; external;
function webkit_dom_html_media_element_get_loop(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_muted(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_network_state(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gushort; cdecl; external;
function webkit_dom_html_media_element_get_paused(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_playback_rate(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gfloat; cdecl; external;
function webkit_dom_html_media_element_get_played(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): PWebKitDOMTimeRanges; cdecl; external;
function webkit_dom_html_media_element_get_preload(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): Pgchar; cdecl; external;
function webkit_dom_html_media_element_get_ready_state(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gushort; cdecl; external;
function webkit_dom_html_media_element_get_seekable(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): PWebKitDOMTimeRanges; cdecl; external;
function webkit_dom_html_media_element_get_seeking(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_src(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): Pgchar; cdecl; external;
function webkit_dom_html_media_element_get_start_time(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gfloat; cdecl; external;
function webkit_dom_html_media_element_get_type: TGType; cdecl; external;
function webkit_dom_html_media_element_get_volume(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gfloat; cdecl; external;
function webkit_dom_html_media_element_get_webkit_closed_captions_visible(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_webkit_has_closed_captions(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_media_element_get_webkit_preserves_pitch(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement): gboolean; cdecl; external;
function webkit_dom_html_menu_element_get_compact(ADOMHTMLMenuElement: PWebKitDOMHTMLMenuElement): gboolean; cdecl; external;
function webkit_dom_html_menu_element_get_type: TGType; cdecl; external;
function webkit_dom_html_meta_element_get_content(ADOMHTMLMetaElement: PWebKitDOMHTMLMetaElement): Pgchar; cdecl; external;
function webkit_dom_html_meta_element_get_http_equiv(ADOMHTMLMetaElement: PWebKitDOMHTMLMetaElement): Pgchar; cdecl; external;
function webkit_dom_html_meta_element_get_name(ADOMHTMLMetaElement: PWebKitDOMHTMLMetaElement): Pgchar; cdecl; external;
function webkit_dom_html_meta_element_get_scheme(ADOMHTMLMetaElement: PWebKitDOMHTMLMetaElement): Pgchar; cdecl; external;
function webkit_dom_html_meta_element_get_type: TGType; cdecl; external;
function webkit_dom_html_mod_element_get_cite(ADOMHTMLModElement: PWebKitDOMHTMLModElement): Pgchar; cdecl; external;
function webkit_dom_html_mod_element_get_date_time(ADOMHTMLModElement: PWebKitDOMHTMLModElement): Pgchar; cdecl; external;
function webkit_dom_html_mod_element_get_type: TGType; cdecl; external;
function webkit_dom_html_object_element_check_validity(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): gboolean; cdecl; external;
function webkit_dom_html_object_element_get_align(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_archive(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_border(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_code(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_code_base(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_code_type(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_content_document(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): PWebKitDOMDocument; cdecl; external;
function webkit_dom_html_object_element_get_data(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_declare(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): gboolean; cdecl; external;
function webkit_dom_html_object_element_get_form(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_object_element_get_height(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_hspace(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): glong; cdecl; external;
function webkit_dom_html_object_element_get_name(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_standby(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_type: TGType; cdecl; external;
function webkit_dom_html_object_element_get_use_map(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_validation_message(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_validity(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): PWebKitDOMValidityState; cdecl; external;
function webkit_dom_html_object_element_get_vspace(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): glong; cdecl; external;
function webkit_dom_html_object_element_get_width(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): Pgchar; cdecl; external;
function webkit_dom_html_object_element_get_will_validate(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement): gboolean; cdecl; external;
function webkit_dom_html_opt_group_element_get_disabled(ADOMHTMLOptGroupElement: PWebKitDOMHTMLOptGroupElement): gboolean; cdecl; external;
function webkit_dom_html_opt_group_element_get_label(ADOMHTMLOptGroupElement: PWebKitDOMHTMLOptGroupElement): Pgchar; cdecl; external;
function webkit_dom_html_opt_group_element_get_type: TGType; cdecl; external;
function webkit_dom_html_option_element_get_default_selected(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement): gboolean; cdecl; external;
function webkit_dom_html_option_element_get_disabled(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement): gboolean; cdecl; external;
function webkit_dom_html_option_element_get_form(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_option_element_get_index(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement): glong; cdecl; external;
function webkit_dom_html_option_element_get_label(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement): Pgchar; cdecl; external;
function webkit_dom_html_option_element_get_selected(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement): gboolean; cdecl; external;
function webkit_dom_html_option_element_get_text(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement): Pgchar; cdecl; external;
function webkit_dom_html_option_element_get_type: TGType; cdecl; external;
function webkit_dom_html_option_element_get_value(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement): Pgchar; cdecl; external;
function webkit_dom_html_options_collection_get_selected_index(ADOMHTMLOptionsCollection: PWebKitDOMHTMLOptionsCollection): glong; cdecl; external;
function webkit_dom_html_options_collection_get_type: TGType; cdecl; external;
function webkit_dom_html_paragraph_element_get_align(ADOMHTMLParagraphElement: PWebKitDOMHTMLParagraphElement): Pgchar; cdecl; external;
function webkit_dom_html_paragraph_element_get_type: TGType; cdecl; external;
function webkit_dom_html_param_element_get_name(ADOMHTMLParamElement: PWebKitDOMHTMLParamElement): Pgchar; cdecl; external;
function webkit_dom_html_param_element_get_type: TGType; cdecl; external;
function webkit_dom_html_param_element_get_value(ADOMHTMLParamElement: PWebKitDOMHTMLParamElement): Pgchar; cdecl; external;
function webkit_dom_html_param_element_get_value_type(ADOMHTMLParamElement: PWebKitDOMHTMLParamElement): Pgchar; cdecl; external;
function webkit_dom_html_pre_element_get_type: TGType; cdecl; external;
function webkit_dom_html_pre_element_get_width(ADOMHTMLPreElement: PWebKitDOMHTMLPreElement): glong; cdecl; external;
function webkit_dom_html_pre_element_get_wrap(ADOMHTMLPreElement: PWebKitDOMHTMLPreElement): gboolean; cdecl; external;
function webkit_dom_html_quote_element_get_cite(ADOMHTMLQuoteElement: PWebKitDOMHTMLQuoteElement): Pgchar; cdecl; external;
function webkit_dom_html_quote_element_get_type: TGType; cdecl; external;
function webkit_dom_html_script_element_get_async(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement): gboolean; cdecl; external;
function webkit_dom_html_script_element_get_charset(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement): Pgchar; cdecl; external;
function webkit_dom_html_script_element_get_defer(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement): gboolean; cdecl; external;
function webkit_dom_html_script_element_get_event(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement): Pgchar; cdecl; external;
function webkit_dom_html_script_element_get_html_for(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement): Pgchar; cdecl; external;
function webkit_dom_html_script_element_get_src(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement): Pgchar; cdecl; external;
function webkit_dom_html_script_element_get_text(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement): Pgchar; cdecl; external;
function webkit_dom_html_script_element_get_type: TGType; cdecl; external;
function webkit_dom_html_select_element_check_validity(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): gboolean; cdecl; external;
function webkit_dom_html_select_element_get_autofocus(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): gboolean; cdecl; external;
function webkit_dom_html_select_element_get_disabled(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): gboolean; cdecl; external;
function webkit_dom_html_select_element_get_form(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_select_element_get_labels(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_html_select_element_get_length(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): gulong; cdecl; external;
function webkit_dom_html_select_element_get_multiple(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): gboolean; cdecl; external;
function webkit_dom_html_select_element_get_name(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): Pgchar; cdecl; external;
function webkit_dom_html_select_element_get_options(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): PWebKitDOMHTMLOptionsCollection; cdecl; external;
function webkit_dom_html_select_element_get_required(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): gboolean; cdecl; external;
function webkit_dom_html_select_element_get_selected_index(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): glong; cdecl; external;
function webkit_dom_html_select_element_get_size(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): glong; cdecl; external;
function webkit_dom_html_select_element_get_type: TGType; cdecl; external;
function webkit_dom_html_select_element_get_validation_message(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): Pgchar; cdecl; external;
function webkit_dom_html_select_element_get_validity(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): PWebKitDOMValidityState; cdecl; external;
function webkit_dom_html_select_element_get_value(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): Pgchar; cdecl; external;
function webkit_dom_html_select_element_get_will_validate(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement): gboolean; cdecl; external;
function webkit_dom_html_select_element_item(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; index: gulong): PWebKitDOMNode; cdecl; external;
function webkit_dom_html_select_element_named_item(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; name: Pgchar): PWebKitDOMNode; cdecl; external;
function webkit_dom_html_style_element_get_disabled(ADOMHTMLStyleElement: PWebKitDOMHTMLStyleElement): gboolean; cdecl; external;
function webkit_dom_html_style_element_get_media(ADOMHTMLStyleElement: PWebKitDOMHTMLStyleElement): Pgchar; cdecl; external;
function webkit_dom_html_style_element_get_sheet(ADOMHTMLStyleElement: PWebKitDOMHTMLStyleElement): PWebKitDOMStyleSheet; cdecl; external;
function webkit_dom_html_style_element_get_type: TGType; cdecl; external;
function webkit_dom_html_table_caption_element_get_align(ADOMHTMLTableCaptionElement: PWebKitDOMHTMLTableCaptionElement): Pgchar; cdecl; external;
function webkit_dom_html_table_caption_element_get_type: TGType; cdecl; external;
function webkit_dom_html_table_cell_element_get_abbr(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_align(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_axis(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_bg_color(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_cell_index(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): glong; cdecl; external;
function webkit_dom_html_table_cell_element_get_ch(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_ch_off(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_col_span(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): glong; cdecl; external;
function webkit_dom_html_table_cell_element_get_headers(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_height(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_no_wrap(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): gboolean; cdecl; external;
function webkit_dom_html_table_cell_element_get_row_span(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): glong; cdecl; external;
function webkit_dom_html_table_cell_element_get_scope(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_type: TGType; cdecl; external;
function webkit_dom_html_table_cell_element_get_v_align(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_cell_element_get_width(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement): Pgchar; cdecl; external;
function webkit_dom_html_table_col_element_get_align(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement): Pgchar; cdecl; external;
function webkit_dom_html_table_col_element_get_ch(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement): Pgchar; cdecl; external;
function webkit_dom_html_table_col_element_get_ch_off(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement): Pgchar; cdecl; external;
function webkit_dom_html_table_col_element_get_span(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement): glong; cdecl; external;
function webkit_dom_html_table_col_element_get_type: TGType; cdecl; external;
function webkit_dom_html_table_col_element_get_v_align(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement): Pgchar; cdecl; external;
function webkit_dom_html_table_col_element_get_width(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_create_caption(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_html_table_element_create_t_foot(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_html_table_element_create_t_head(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_html_table_element_get_align(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_get_bg_color(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_get_border(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_get_caption(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): PWebKitDOMHTMLTableCaptionElement; cdecl; external;
function webkit_dom_html_table_element_get_cell_padding(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_get_cell_spacing(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_get_frame(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_get_rows(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_table_element_get_rules(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_get_summary(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_get_t_bodies(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_table_element_get_t_foot(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): PWebKitDOMHTMLTableSectionElement; cdecl; external;
function webkit_dom_html_table_element_get_t_head(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): PWebKitDOMHTMLTableSectionElement; cdecl; external;
function webkit_dom_html_table_element_get_type: TGType; cdecl; external;
function webkit_dom_html_table_element_get_width(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement): Pgchar; cdecl; external;
function webkit_dom_html_table_element_insert_row(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; index: glong): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_html_table_row_element_get_align(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement): Pgchar; cdecl; external;
function webkit_dom_html_table_row_element_get_bg_color(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement): Pgchar; cdecl; external;
function webkit_dom_html_table_row_element_get_cells(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_table_row_element_get_ch(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement): Pgchar; cdecl; external;
function webkit_dom_html_table_row_element_get_ch_off(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement): Pgchar; cdecl; external;
function webkit_dom_html_table_row_element_get_row_index(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement): glong; cdecl; external;
function webkit_dom_html_table_row_element_get_section_row_index(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement): glong; cdecl; external;
function webkit_dom_html_table_row_element_get_type: TGType; cdecl; external;
function webkit_dom_html_table_row_element_get_v_align(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement): Pgchar; cdecl; external;
function webkit_dom_html_table_row_element_insert_cell(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement; index: glong): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_html_table_section_element_get_align(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement): Pgchar; cdecl; external;
function webkit_dom_html_table_section_element_get_ch(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement): Pgchar; cdecl; external;
function webkit_dom_html_table_section_element_get_ch_off(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement): Pgchar; cdecl; external;
function webkit_dom_html_table_section_element_get_rows(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement): PWebKitDOMHTMLCollection; cdecl; external;
function webkit_dom_html_table_section_element_get_type: TGType; cdecl; external;
function webkit_dom_html_table_section_element_get_v_align(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement): Pgchar; cdecl; external;
function webkit_dom_html_table_section_element_insert_row(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement; index: glong): PWebKitDOMHTMLElement; cdecl; external;
function webkit_dom_html_text_area_element_check_validity(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): gboolean; cdecl; external;
function webkit_dom_html_text_area_element_get_access_key(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_text_area_element_get_autofocus(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): gboolean; cdecl; external;
function webkit_dom_html_text_area_element_get_cols(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): glong; cdecl; external;
function webkit_dom_html_text_area_element_get_default_value(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_text_area_element_get_disabled(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): gboolean; cdecl; external;
function webkit_dom_html_text_area_element_get_form(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): PWebKitDOMHTMLFormElement; cdecl; external;
function webkit_dom_html_text_area_element_get_labels(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_html_text_area_element_get_max_length(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): glong; cdecl; external;
function webkit_dom_html_text_area_element_get_name(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_text_area_element_get_placeholder(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_text_area_element_get_read_only(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): gboolean; cdecl; external;
function webkit_dom_html_text_area_element_get_required(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): gboolean; cdecl; external;
function webkit_dom_html_text_area_element_get_rows(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): glong; cdecl; external;
function webkit_dom_html_text_area_element_get_selection_end(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): glong; cdecl; external;
function webkit_dom_html_text_area_element_get_selection_start(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): glong; cdecl; external;
function webkit_dom_html_text_area_element_get_text_length(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): gulong; cdecl; external;
function webkit_dom_html_text_area_element_get_type: TGType; cdecl; external;
function webkit_dom_html_text_area_element_get_validation_message(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_text_area_element_get_validity(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): PWebKitDOMValidityState; cdecl; external;
function webkit_dom_html_text_area_element_get_value(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): Pgchar; cdecl; external;
function webkit_dom_html_text_area_element_get_will_validate(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement): gboolean; cdecl; external;
function webkit_dom_html_title_element_get_text(ADOMHTMLTitleElement: PWebKitDOMHTMLTitleElement): Pgchar; cdecl; external;
function webkit_dom_html_title_element_get_type: TGType; cdecl; external;
function webkit_dom_html_video_element_get_height(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement): gulong; cdecl; external;
function webkit_dom_html_video_element_get_poster(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement): Pgchar; cdecl; external;
function webkit_dom_html_video_element_get_type: TGType; cdecl; external;
function webkit_dom_html_video_element_get_video_height(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement): gulong; cdecl; external;
function webkit_dom_html_video_element_get_video_width(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement): gulong; cdecl; external;
function webkit_dom_html_video_element_get_webkit_displaying_fullscreen(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement): gboolean; cdecl; external;
function webkit_dom_html_video_element_get_webkit_supports_fullscreen(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement): gboolean; cdecl; external;
function webkit_dom_html_video_element_get_width(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement): gulong; cdecl; external;
function webkit_dom_htmlbr_element_get_clear(ADOMHTMLBRElement: PWebKitDOMHTMLBRElement): Pgchar; cdecl; external;
function webkit_dom_htmlbr_element_get_type: TGType; cdecl; external;
function webkit_dom_htmld_list_element_get_compact(ADOMHTMLDListElement: PWebKitDOMHTMLDListElement): gboolean; cdecl; external;
function webkit_dom_htmld_list_element_get_type: TGType; cdecl; external;
function webkit_dom_htmlhr_element_get_align(ADOMHTMLHRElement: PWebKitDOMHTMLHRElement): Pgchar; cdecl; external;
function webkit_dom_htmlhr_element_get_no_shade(ADOMHTMLHRElement: PWebKitDOMHTMLHRElement): gboolean; cdecl; external;
function webkit_dom_htmlhr_element_get_size(ADOMHTMLHRElement: PWebKitDOMHTMLHRElement): Pgchar; cdecl; external;
function webkit_dom_htmlhr_element_get_type: TGType; cdecl; external;
function webkit_dom_htmlhr_element_get_width(ADOMHTMLHRElement: PWebKitDOMHTMLHRElement): Pgchar; cdecl; external;
function webkit_dom_htmlli_element_get_type: TGType; cdecl; external;
function webkit_dom_htmlli_element_get_value(ADOMHTMLLIElement: PWebKitDOMHTMLLIElement): glong; cdecl; external;
function webkit_dom_htmlo_list_element_get_compact(ADOMHTMLOListElement: PWebKitDOMHTMLOListElement): gboolean; cdecl; external;
function webkit_dom_htmlo_list_element_get_start(ADOMHTMLOListElement: PWebKitDOMHTMLOListElement): glong; cdecl; external;
function webkit_dom_htmlo_list_element_get_type: TGType; cdecl; external;
function webkit_dom_htmlu_list_element_get_compact(ADOMHTMLUListElement: PWebKitDOMHTMLUListElement): gboolean; cdecl; external;
function webkit_dom_htmlu_list_element_get_type: TGType; cdecl; external;
function webkit_dom_location_get_origin(ADOMLocation: PWebKitDOMLocation): Pgchar; cdecl; external;
function webkit_dom_location_get_parameter(ADOMLocation: PWebKitDOMLocation; name: Pgchar): Pgchar; cdecl; external;
function webkit_dom_location_get_type: TGType; cdecl; external;
function webkit_dom_media_error_get_code(ADOMMediaError: PWebKitDOMMediaError): gushort; cdecl; external;
function webkit_dom_media_error_get_type: TGType; cdecl; external;
function webkit_dom_media_list_get_length(ADOMMediaList: PWebKitDOMMediaList): gulong; cdecl; external;
function webkit_dom_media_list_get_media_text(ADOMMediaList: PWebKitDOMMediaList): Pgchar; cdecl; external;
function webkit_dom_media_list_get_type: TGType; cdecl; external;
function webkit_dom_media_list_item(ADOMMediaList: PWebKitDOMMediaList; index: gulong): Pgchar; cdecl; external;
function webkit_dom_media_query_list_get_matches(ADOMMediaQueryList: PWebKitDOMMediaQueryList): gboolean; cdecl; external;
function webkit_dom_media_query_list_get_media(ADOMMediaQueryList: PWebKitDOMMediaQueryList): Pgchar; cdecl; external;
function webkit_dom_media_query_list_get_type: TGType; cdecl; external;
function webkit_dom_memory_info_get_js_heap_size_limit(ADOMMemoryInfo: PWebKitDOMMemoryInfo): gulong; cdecl; external;
function webkit_dom_memory_info_get_total_js_heap_size(ADOMMemoryInfo: PWebKitDOMMemoryInfo): gulong; cdecl; external;
function webkit_dom_memory_info_get_type: TGType; cdecl; external;
function webkit_dom_memory_info_get_used_js_heap_size(ADOMMemoryInfo: PWebKitDOMMemoryInfo): gulong; cdecl; external;
function webkit_dom_message_port_get_type: TGType; cdecl; external;
function webkit_dom_mouse_event_get_alt_key(ADOMMouseEvent: PWebKitDOMMouseEvent): gboolean; cdecl; external;
function webkit_dom_mouse_event_get_button(ADOMMouseEvent: PWebKitDOMMouseEvent): gushort; cdecl; external;
function webkit_dom_mouse_event_get_client_x(ADOMMouseEvent: PWebKitDOMMouseEvent): glong; cdecl; external;
function webkit_dom_mouse_event_get_client_y(ADOMMouseEvent: PWebKitDOMMouseEvent): glong; cdecl; external;
function webkit_dom_mouse_event_get_ctrl_key(ADOMMouseEvent: PWebKitDOMMouseEvent): gboolean; cdecl; external;
function webkit_dom_mouse_event_get_from_element(ADOMMouseEvent: PWebKitDOMMouseEvent): PWebKitDOMNode; cdecl; external;
function webkit_dom_mouse_event_get_meta_key(ADOMMouseEvent: PWebKitDOMMouseEvent): gboolean; cdecl; external;
function webkit_dom_mouse_event_get_offset_x(ADOMMouseEvent: PWebKitDOMMouseEvent): glong; cdecl; external;
function webkit_dom_mouse_event_get_offset_y(ADOMMouseEvent: PWebKitDOMMouseEvent): glong; cdecl; external;
function webkit_dom_mouse_event_get_related_target(ADOMMouseEvent: PWebKitDOMMouseEvent): PWebKitDOMEventTarget; cdecl; external;
function webkit_dom_mouse_event_get_screen_x(ADOMMouseEvent: PWebKitDOMMouseEvent): glong; cdecl; external;
function webkit_dom_mouse_event_get_screen_y(ADOMMouseEvent: PWebKitDOMMouseEvent): glong; cdecl; external;
function webkit_dom_mouse_event_get_shift_key(ADOMMouseEvent: PWebKitDOMMouseEvent): gboolean; cdecl; external;
function webkit_dom_mouse_event_get_to_element(ADOMMouseEvent: PWebKitDOMMouseEvent): PWebKitDOMNode; cdecl; external;
function webkit_dom_mouse_event_get_type: TGType; cdecl; external;
function webkit_dom_mouse_event_get_x(ADOMMouseEvent: PWebKitDOMMouseEvent): glong; cdecl; external;
function webkit_dom_mouse_event_get_y(ADOMMouseEvent: PWebKitDOMMouseEvent): glong; cdecl; external;
function webkit_dom_named_node_map_get_length(ADOMNamedNodeMap: PWebKitDOMNamedNodeMap): gulong; cdecl; external;
function webkit_dom_named_node_map_get_named_item(ADOMNamedNodeMap: PWebKitDOMNamedNodeMap; name: Pgchar): PWebKitDOMNode; cdecl; external;
function webkit_dom_named_node_map_get_named_item_ns(ADOMNamedNodeMap: PWebKitDOMNamedNodeMap; namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNode; cdecl; external;
function webkit_dom_named_node_map_get_type: TGType; cdecl; external;
function webkit_dom_named_node_map_item(ADOMNamedNodeMap: PWebKitDOMNamedNodeMap; index: gulong): PWebKitDOMNode; cdecl; external;
function webkit_dom_named_node_map_remove_named_item(ADOMNamedNodeMap: PWebKitDOMNamedNodeMap; name: Pgchar): PWebKitDOMNode; cdecl; external;
function webkit_dom_named_node_map_remove_named_item_ns(ADOMNamedNodeMap: PWebKitDOMNamedNodeMap; namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNode; cdecl; external;
function webkit_dom_named_node_map_set_named_item(ADOMNamedNodeMap: PWebKitDOMNamedNodeMap; node: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_named_node_map_set_named_item_ns(ADOMNamedNodeMap: PWebKitDOMNamedNodeMap; node: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_navigator_get_app_code_name(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_app_name(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_app_version(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_cookie_enabled(ADOMNavigator: PWebKitDOMNavigator): gboolean; cdecl; external;
function webkit_dom_navigator_get_language(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_mime_types(ADOMNavigator: PWebKitDOMNavigator): PWebKitDOMDOMMimeTypeArray; cdecl; external;
function webkit_dom_navigator_get_on_line(ADOMNavigator: PWebKitDOMNavigator): gboolean; cdecl; external;
function webkit_dom_navigator_get_platform(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_plugins(ADOMNavigator: PWebKitDOMNavigator): PWebKitDOMDOMPluginArray; cdecl; external;
function webkit_dom_navigator_get_product(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_product_sub(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_type: TGType; cdecl; external;
function webkit_dom_navigator_get_user_agent(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_vendor(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_get_vendor_sub(ADOMNavigator: PWebKitDOMNavigator): Pgchar; cdecl; external;
function webkit_dom_navigator_java_enabled(ADOMNavigator: PWebKitDOMNavigator): gboolean; cdecl; external;
function webkit_dom_node_append_child(ADOMNode: PWebKitDOMNode; new_child: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_clone_node(ADOMNode: PWebKitDOMNode; deep: gboolean): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_compare_document_position(ADOMNode: PWebKitDOMNode; other: PWebKitDOMNode): gushort; cdecl; external;
function webkit_dom_node_dispatch_event(ADOMNode: PWebKitDOMNode; event: PWebKitDOMEvent): gboolean; cdecl; external;
function webkit_dom_node_filter_accept_node(ADOMNodeFilter: PWebKitDOMNodeFilter; n: PWebKitDOMNode): Tgshort; cdecl; external;
function webkit_dom_node_filter_get_type: TGType; cdecl; external;
function webkit_dom_node_get_attributes(ADOMNode: PWebKitDOMNode): PWebKitDOMNamedNodeMap; cdecl; external;
function webkit_dom_node_get_base_uri(ADOMNode: PWebKitDOMNode): Pgchar; cdecl; external;
function webkit_dom_node_get_child_nodes(ADOMNode: PWebKitDOMNode): PWebKitDOMNodeList; cdecl; external;
function webkit_dom_node_get_first_child(ADOMNode: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_get_last_child(ADOMNode: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_get_local_name(ADOMNode: PWebKitDOMNode): Pgchar; cdecl; external;
function webkit_dom_node_get_namespace_uri(ADOMNode: PWebKitDOMNode): Pgchar; cdecl; external;
function webkit_dom_node_get_next_sibling(ADOMNode: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_get_node_name(ADOMNode: PWebKitDOMNode): Pgchar; cdecl; external;
function webkit_dom_node_get_node_type(ADOMNode: PWebKitDOMNode): gushort; cdecl; external;
function webkit_dom_node_get_node_value(ADOMNode: PWebKitDOMNode): Pgchar; cdecl; external;
function webkit_dom_node_get_owner_document(ADOMNode: PWebKitDOMNode): PWebKitDOMDocument; cdecl; external;
function webkit_dom_node_get_parent_element(ADOMNode: PWebKitDOMNode): PWebKitDOMElement; cdecl; external;
function webkit_dom_node_get_parent_node(ADOMNode: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_get_prefix(ADOMNode: PWebKitDOMNode): Pgchar; cdecl; external;
function webkit_dom_node_get_previous_sibling(ADOMNode: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_get_text_content(ADOMNode: PWebKitDOMNode): Pgchar; cdecl; external;
function webkit_dom_node_get_type: TGType; cdecl; external;
function webkit_dom_node_has_attributes(ADOMNode: PWebKitDOMNode): gboolean; cdecl; external;
function webkit_dom_node_has_child_nodes(ADOMNode: PWebKitDOMNode): gboolean; cdecl; external;
function webkit_dom_node_insert_before(ADOMNode: PWebKitDOMNode; new_child: PWebKitDOMNode; ref_child: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_is_default_namespace(ADOMNode: PWebKitDOMNode; namespace_uri: Pgchar): gboolean; cdecl; external;
function webkit_dom_node_is_equal_node(ADOMNode: PWebKitDOMNode; other: PWebKitDOMNode): gboolean; cdecl; external;
function webkit_dom_node_is_same_node(ADOMNode: PWebKitDOMNode; other: PWebKitDOMNode): gboolean; cdecl; external;
function webkit_dom_node_is_supported(ADOMNode: PWebKitDOMNode; feature: Pgchar; version: Pgchar): gboolean; cdecl; external;
function webkit_dom_node_iterator_get_expand_entity_references(ADOMNodeIterator: PWebKitDOMNodeIterator): gboolean; cdecl; external;
function webkit_dom_node_iterator_get_filter(ADOMNodeIterator: PWebKitDOMNodeIterator): PWebKitDOMNodeFilter; cdecl; external;
function webkit_dom_node_iterator_get_pointer_before_reference_node(ADOMNodeIterator: PWebKitDOMNodeIterator): gboolean; cdecl; external;
function webkit_dom_node_iterator_get_reference_node(ADOMNodeIterator: PWebKitDOMNodeIterator): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_iterator_get_root(ADOMNodeIterator: PWebKitDOMNodeIterator): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_iterator_get_type: TGType; cdecl; external;
function webkit_dom_node_iterator_get_what_to_show(ADOMNodeIterator: PWebKitDOMNodeIterator): gulong; cdecl; external;
function webkit_dom_node_iterator_next_node(ADOMNodeIterator: PWebKitDOMNodeIterator): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_iterator_previous_node(ADOMNodeIterator: PWebKitDOMNodeIterator): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_list_get_length(ADOMNodeList: PWebKitDOMNodeList): gulong; cdecl; external;
function webkit_dom_node_list_get_type: TGType; cdecl; external;
function webkit_dom_node_list_item(ADOMNodeList: PWebKitDOMNodeList; index: gulong): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_lookup_namespace_uri(ADOMNode: PWebKitDOMNode; prefix: Pgchar): Pgchar; cdecl; external;
function webkit_dom_node_lookup_prefix(ADOMNode: PWebKitDOMNode; namespace_uri: Pgchar): Pgchar; cdecl; external;
function webkit_dom_node_remove_child(ADOMNode: PWebKitDOMNode; old_child: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_node_replace_child(ADOMNode: PWebKitDOMNode; new_child: PWebKitDOMNode; old_child: PWebKitDOMNode): PWebKitDOMNode; cdecl; external;
function webkit_dom_object_get_type: TGType; cdecl; external;
function webkit_dom_processing_instruction_get_data(ADOMProcessingInstruction: PWebKitDOMProcessingInstruction): Pgchar; cdecl; external;
function webkit_dom_processing_instruction_get_sheet(ADOMProcessingInstruction: PWebKitDOMProcessingInstruction): PWebKitDOMStyleSheet; cdecl; external;
function webkit_dom_processing_instruction_get_target(ADOMProcessingInstruction: PWebKitDOMProcessingInstruction): Pgchar; cdecl; external;
function webkit_dom_processing_instruction_get_type: TGType; cdecl; external;
function webkit_dom_range_clone_contents(ADOMRange: PWebKitDOMRange): PWebKitDOMDocumentFragment; cdecl; external;
function webkit_dom_range_clone_range(ADOMRange: PWebKitDOMRange): PWebKitDOMRange; cdecl; external;
function webkit_dom_range_compare_boundary_points(ADOMRange: PWebKitDOMRange; how: gushort; source_range: PWebKitDOMRange): Tgshort; cdecl; external;
function webkit_dom_range_compare_node(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode): Tgshort; cdecl; external;
function webkit_dom_range_compare_point(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode; offset: glong): Tgshort; cdecl; external;
function webkit_dom_range_create_contextual_fragment(ADOMRange: PWebKitDOMRange; html: Pgchar): PWebKitDOMDocumentFragment; cdecl; external;
function webkit_dom_range_extract_contents(ADOMRange: PWebKitDOMRange): PWebKitDOMDocumentFragment; cdecl; external;
function webkit_dom_range_get_collapsed(ADOMRange: PWebKitDOMRange): gboolean; cdecl; external;
function webkit_dom_range_get_common_ancestor_container(ADOMRange: PWebKitDOMRange): PWebKitDOMNode; cdecl; external;
function webkit_dom_range_get_end_container(ADOMRange: PWebKitDOMRange): PWebKitDOMNode; cdecl; external;
function webkit_dom_range_get_end_offset(ADOMRange: PWebKitDOMRange): glong; cdecl; external;
function webkit_dom_range_get_start_container(ADOMRange: PWebKitDOMRange): PWebKitDOMNode; cdecl; external;
function webkit_dom_range_get_start_offset(ADOMRange: PWebKitDOMRange): glong; cdecl; external;
function webkit_dom_range_get_text(ADOMRange: PWebKitDOMRange): Pgchar; cdecl; external;
function webkit_dom_range_get_type: TGType; cdecl; external;
function webkit_dom_range_intersects_node(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode): gboolean; cdecl; external;
function webkit_dom_range_is_point_in_range(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode; offset: glong): gboolean; cdecl; external;
function webkit_dom_range_to_string(ADOMRange: PWebKitDOMRange): Pgchar; cdecl; external;
function webkit_dom_screen_get_avail_height(ADOMScreen: PWebKitDOMScreen): gulong; cdecl; external;
function webkit_dom_screen_get_avail_left(ADOMScreen: PWebKitDOMScreen): glong; cdecl; external;
function webkit_dom_screen_get_avail_top(ADOMScreen: PWebKitDOMScreen): glong; cdecl; external;
function webkit_dom_screen_get_avail_width(ADOMScreen: PWebKitDOMScreen): gulong; cdecl; external;
function webkit_dom_screen_get_color_depth(ADOMScreen: PWebKitDOMScreen): gulong; cdecl; external;
function webkit_dom_screen_get_height(ADOMScreen: PWebKitDOMScreen): gulong; cdecl; external;
function webkit_dom_screen_get_pixel_depth(ADOMScreen: PWebKitDOMScreen): gulong; cdecl; external;
function webkit_dom_screen_get_type: TGType; cdecl; external;
function webkit_dom_screen_get_width(ADOMScreen: PWebKitDOMScreen): gulong; cdecl; external;
function webkit_dom_storage_get_item(ADOMStorage: PWebKitDOMStorage; key: Pgchar): Pgchar; cdecl; external;
function webkit_dom_storage_get_length(ADOMStorage: PWebKitDOMStorage): gulong; cdecl; external;
function webkit_dom_storage_get_type: TGType; cdecl; external;
function webkit_dom_storage_key(ADOMStorage: PWebKitDOMStorage; index: gulong): Pgchar; cdecl; external;
function webkit_dom_style_media_get_type: TGType; cdecl; external;
function webkit_dom_style_media_match_medium(ADOMStyleMedia: PWebKitDOMStyleMedia; mediaquery: Pgchar): gboolean; cdecl; external;
function webkit_dom_style_sheet_get_disabled(ADOMStyleSheet: PWebKitDOMStyleSheet): gboolean; cdecl; external;
function webkit_dom_style_sheet_get_href(ADOMStyleSheet: PWebKitDOMStyleSheet): Pgchar; cdecl; external;
function webkit_dom_style_sheet_get_media(ADOMStyleSheet: PWebKitDOMStyleSheet): PWebKitDOMMediaList; cdecl; external;
function webkit_dom_style_sheet_get_owner_node(ADOMStyleSheet: PWebKitDOMStyleSheet): PWebKitDOMNode; cdecl; external;
function webkit_dom_style_sheet_get_parent_style_sheet(ADOMStyleSheet: PWebKitDOMStyleSheet): PWebKitDOMStyleSheet; cdecl; external;
function webkit_dom_style_sheet_get_title(ADOMStyleSheet: PWebKitDOMStyleSheet): Pgchar; cdecl; external;
function webkit_dom_style_sheet_get_type: TGType; cdecl; external;
function webkit_dom_style_sheet_list_get_length(ADOMStyleSheetList: PWebKitDOMStyleSheetList): gulong; cdecl; external;
function webkit_dom_style_sheet_list_get_type: TGType; cdecl; external;
function webkit_dom_style_sheet_list_item(ADOMStyleSheetList: PWebKitDOMStyleSheetList; index: gulong): PWebKitDOMStyleSheet; cdecl; external;
function webkit_dom_text_get_type: TGType; cdecl; external;
function webkit_dom_text_get_whole_text(ADOMText: PWebKitDOMText): Pgchar; cdecl; external;
function webkit_dom_text_replace_whole_text(ADOMText: PWebKitDOMText; content: Pgchar): PWebKitDOMText; cdecl; external;
function webkit_dom_text_split_text(ADOMText: PWebKitDOMText; offset: gulong): PWebKitDOMText; cdecl; external;
function webkit_dom_time_ranges_end(ADOMTimeRanges: PWebKitDOMTimeRanges; index: gulong): gfloat; cdecl; external;
function webkit_dom_time_ranges_get_length(ADOMTimeRanges: PWebKitDOMTimeRanges): gulong; cdecl; external;
function webkit_dom_time_ranges_get_type: TGType; cdecl; external;
function webkit_dom_time_ranges_start(ADOMTimeRanges: PWebKitDOMTimeRanges; index: gulong): gfloat; cdecl; external;
function webkit_dom_tree_walker_first_child(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_tree_walker_get_current_node(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_tree_walker_get_expand_entity_references(ADOMTreeWalker: PWebKitDOMTreeWalker): gboolean; cdecl; external;
function webkit_dom_tree_walker_get_filter(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNodeFilter; cdecl; external;
function webkit_dom_tree_walker_get_root(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_tree_walker_get_type: TGType; cdecl; external;
function webkit_dom_tree_walker_get_what_to_show(ADOMTreeWalker: PWebKitDOMTreeWalker): gulong; cdecl; external;
function webkit_dom_tree_walker_last_child(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_tree_walker_next_node(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_tree_walker_next_sibling(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_tree_walker_parent_node(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_tree_walker_previous_node(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_tree_walker_previous_sibling(ADOMTreeWalker: PWebKitDOMTreeWalker): PWebKitDOMNode; cdecl; external;
function webkit_dom_ui_event_get_char_code(ADOMUIEvent: PWebKitDOMUIEvent): glong; cdecl; external;
function webkit_dom_ui_event_get_detail(ADOMUIEvent: PWebKitDOMUIEvent): glong; cdecl; external;
function webkit_dom_ui_event_get_key_code(ADOMUIEvent: PWebKitDOMUIEvent): glong; cdecl; external;
function webkit_dom_ui_event_get_layer_x(ADOMUIEvent: PWebKitDOMUIEvent): glong; cdecl; external;
function webkit_dom_ui_event_get_layer_y(ADOMUIEvent: PWebKitDOMUIEvent): glong; cdecl; external;
function webkit_dom_ui_event_get_page_x(ADOMUIEvent: PWebKitDOMUIEvent): glong; cdecl; external;
function webkit_dom_ui_event_get_page_y(ADOMUIEvent: PWebKitDOMUIEvent): glong; cdecl; external;
function webkit_dom_ui_event_get_type: TGType; cdecl; external;
function webkit_dom_ui_event_get_view(ADOMUIEvent: PWebKitDOMUIEvent): PWebKitDOMDOMWindow; cdecl; external;
function webkit_dom_ui_event_get_which(ADOMUIEvent: PWebKitDOMUIEvent): glong; cdecl; external;
function webkit_dom_validity_state_get_custom_error(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_validity_state_get_pattern_mismatch(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_validity_state_get_range_overflow(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_validity_state_get_range_underflow(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_validity_state_get_step_mismatch(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_validity_state_get_too_long(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_validity_state_get_type: TGType; cdecl; external;
function webkit_dom_validity_state_get_type_mismatch(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_validity_state_get_valid(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_validity_state_get_value_missing(ADOMValidityState: PWebKitDOMValidityState): gboolean; cdecl; external;
function webkit_dom_webkit_animation_get_delay(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation): gdouble; cdecl; external;
function webkit_dom_webkit_animation_get_direction(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation): gushort; cdecl; external;
function webkit_dom_webkit_animation_get_duration(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation): gdouble; cdecl; external;
function webkit_dom_webkit_animation_get_elapsed_time(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation): gdouble; cdecl; external;
function webkit_dom_webkit_animation_get_ended(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation): gboolean; cdecl; external;
function webkit_dom_webkit_animation_get_fill_mode(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation): gushort; cdecl; external;
function webkit_dom_webkit_animation_get_name(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation): Pgchar; cdecl; external;
function webkit_dom_webkit_animation_get_paused(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation): gboolean; cdecl; external;
function webkit_dom_webkit_animation_get_type: TGType; cdecl; external;
function webkit_dom_webkit_animation_list_get_length(ADOMWebKitAnimationList: PWebKitDOMWebKitAnimationList): gulong; cdecl; external;
function webkit_dom_webkit_animation_list_get_type: TGType; cdecl; external;
function webkit_dom_webkit_animation_list_item(ADOMWebKitAnimationList: PWebKitDOMWebKitAnimationList; index: gulong): PWebKitDOMWebKitAnimation; cdecl; external;
function webkit_dom_webkit_point_get_type: TGType; cdecl; external;
function webkit_dom_webkit_point_get_x(ADOMWebKitPoint: PWebKitDOMWebKitPoint): gfloat; cdecl; external;
function webkit_dom_webkit_point_get_y(ADOMWebKitPoint: PWebKitDOMWebKitPoint): gfloat; cdecl; external;
function webkit_dom_xpath_expression_evaluate(ADOMXPathExpression: PWebKitDOMXPathExpression; context_node: PWebKitDOMNode; type_: gushort; in_result: PWebKitDOMXPathResult): PWebKitDOMXPathResult; cdecl; external;
function webkit_dom_xpath_expression_get_type: TGType; cdecl; external;
function webkit_dom_xpath_ns_resolver_get_type: TGType; cdecl; external;
function webkit_dom_xpath_ns_resolver_lookup_namespace_uri(ADOMXPathNSResolver: PWebKitDOMXPathNSResolver; prefix: Pgchar): Pgchar; cdecl; external;
function webkit_dom_xpath_result_get_boolean_value(ADOMXPathResult: PWebKitDOMXPathResult): gboolean; cdecl; external;
function webkit_dom_xpath_result_get_invalid_iterator_state(ADOMXPathResult: PWebKitDOMXPathResult): gboolean; cdecl; external;
function webkit_dom_xpath_result_get_number_value(ADOMXPathResult: PWebKitDOMXPathResult): gdouble; cdecl; external;
function webkit_dom_xpath_result_get_result_type(ADOMXPathResult: PWebKitDOMXPathResult): gushort; cdecl; external;
function webkit_dom_xpath_result_get_single_node_value(ADOMXPathResult: PWebKitDOMXPathResult): PWebKitDOMNode; cdecl; external;
function webkit_dom_xpath_result_get_snapshot_length(ADOMXPathResult: PWebKitDOMXPathResult): gulong; cdecl; external;
function webkit_dom_xpath_result_get_string_value(ADOMXPathResult: PWebKitDOMXPathResult): Pgchar; cdecl; external;
function webkit_dom_xpath_result_get_type: TGType; cdecl; external;
function webkit_dom_xpath_result_iterate_next(ADOMXPathResult: PWebKitDOMXPathResult): PWebKitDOMNode; cdecl; external;
function webkit_dom_xpath_result_snapshot_item(ADOMXPathResult: PWebKitDOMXPathResult; index: gulong): PWebKitDOMNode; cdecl; external;
function webkit_download_get_current_size(ADownload: PWebKitDownload): guint64; cdecl; external;
function webkit_download_get_destination_uri(ADownload: PWebKitDownload): Pgchar; cdecl; external;
function webkit_download_get_elapsed_time(ADownload: PWebKitDownload): gdouble; cdecl; external;
function webkit_download_get_network_request(ADownload: PWebKitDownload): PWebKitNetworkRequest; cdecl; external;
function webkit_download_get_network_response(ADownload: PWebKitDownload): PWebKitNetworkResponse; cdecl; external;
function webkit_download_get_progress(ADownload: PWebKitDownload): gdouble; cdecl; external;
function webkit_download_get_status(ADownload: PWebKitDownload): TWebKitDownloadStatus; cdecl; external;
function webkit_download_get_suggested_filename(ADownload: PWebKitDownload): Pgchar; cdecl; external;
function webkit_download_get_total_size(ADownload: PWebKitDownload): guint64; cdecl; external;
function webkit_download_get_type: TGType; cdecl; external;
function webkit_download_get_uri(ADownload: PWebKitDownload): Pgchar; cdecl; external;
function webkit_download_new(request: PWebKitNetworkRequest): PWebKitDownload; cdecl; external;
function webkit_geolocation_policy_decision_get_type: TGType; cdecl; external;
function webkit_get_cache_model: TWebKitCacheModel; cdecl; external;
function webkit_get_default_session: PSoupSession; cdecl; external;
function webkit_get_default_web_database_quota: guint64; cdecl; external;
function webkit_get_icon_database: PWebKitIconDatabase; cdecl; external;
function webkit_get_web_database_directory_path: Pgchar; cdecl; external;
function webkit_get_web_plugin_database: PWebKitWebPluginDatabase; cdecl; external;
function webkit_hit_test_result_get_type: TGType; cdecl; external;
function webkit_icon_database_get_icon_pixbuf(AIconDatabase: PWebKitIconDatabase; page_uri: Pgchar): PGdkPixbuf; cdecl; external;
function webkit_icon_database_get_icon_uri(AIconDatabase: PWebKitIconDatabase; page_uri: Pgchar): Pgchar; cdecl; external;
function webkit_icon_database_get_path(AIconDatabase: PWebKitIconDatabase): Pgchar; cdecl; external;
function webkit_icon_database_get_type: TGType; cdecl; external;
function webkit_major_version: guint; cdecl; external;
function webkit_micro_version: guint; cdecl; external;
function webkit_minor_version: guint; cdecl; external;
function webkit_network_error_quark: TGQuark; cdecl; external;
function webkit_network_request_get_message(ANetworkRequest: PWebKitNetworkRequest): PSoupMessage; cdecl; external;
function webkit_network_request_get_type: TGType; cdecl; external;
function webkit_network_request_get_uri(ANetworkRequest: PWebKitNetworkRequest): Pgchar; cdecl; external;
function webkit_network_request_new(uri: Pgchar): PWebKitNetworkRequest; cdecl; external;
function webkit_network_response_get_message(ANetworkResponse: PWebKitNetworkResponse): PSoupMessage; cdecl; external;
function webkit_network_response_get_type: TGType; cdecl; external;
function webkit_network_response_get_uri(ANetworkResponse: PWebKitNetworkResponse): Pgchar; cdecl; external;
function webkit_network_response_new(uri: Pgchar): PWebKitNetworkResponse; cdecl; external;
function webkit_plugin_error_quark: TGQuark; cdecl; external;
function webkit_policy_error_quark: TGQuark; cdecl; external;
function webkit_security_origin_get_all_web_databases(ASecurityOrigin: PWebKitSecurityOrigin): PGList; cdecl; external;
function webkit_security_origin_get_host(ASecurityOrigin: PWebKitSecurityOrigin): Pgchar; cdecl; external;
function webkit_security_origin_get_port(ASecurityOrigin: PWebKitSecurityOrigin): guint; cdecl; external;
function webkit_security_origin_get_protocol(ASecurityOrigin: PWebKitSecurityOrigin): Pgchar; cdecl; external;
function webkit_security_origin_get_type: TGType; cdecl; external;
function webkit_security_origin_get_web_database_quota(ASecurityOrigin: PWebKitSecurityOrigin): guint64; cdecl; external;
function webkit_security_origin_get_web_database_usage(ASecurityOrigin: PWebKitSecurityOrigin): guint64; cdecl; external;
function webkit_soup_auth_dialog_get_type: TGType; cdecl; external;
function webkit_viewport_attributes_get_type: TGType; cdecl; external;
function webkit_web_back_forward_list_contains_item(AWebBackForwardList: PWebKitWebBackForwardList; history_item: TWebKitWebHistoryItem): gboolean; cdecl; external;
function webkit_web_back_forward_list_get_back_item(AWebBackForwardList: PWebKitWebBackForwardList): TWebKitWebHistoryItem; cdecl; external;
function webkit_web_back_forward_list_get_back_length(AWebBackForwardList: PWebKitWebBackForwardList): gint; cdecl; external;
function webkit_web_back_forward_list_get_back_list_with_limit(AWebBackForwardList: PWebKitWebBackForwardList; limit: gint): PGList; cdecl; external;
function webkit_web_back_forward_list_get_current_item(AWebBackForwardList: PWebKitWebBackForwardList): TWebKitWebHistoryItem; cdecl; external;
function webkit_web_back_forward_list_get_forward_item(AWebBackForwardList: PWebKitWebBackForwardList): TWebKitWebHistoryItem; cdecl; external;
function webkit_web_back_forward_list_get_forward_length(AWebBackForwardList: PWebKitWebBackForwardList): gint; cdecl; external;
function webkit_web_back_forward_list_get_forward_list_with_limit(AWebBackForwardList: PWebKitWebBackForwardList; limit: gint): PGList; cdecl; external;
function webkit_web_back_forward_list_get_limit(AWebBackForwardList: PWebKitWebBackForwardList): gint; cdecl; external;
function webkit_web_back_forward_list_get_nth_item(AWebBackForwardList: PWebKitWebBackForwardList; index: gint): TWebKitWebHistoryItem; cdecl; external;
function webkit_web_back_forward_list_get_type: TGType; cdecl; external;
function webkit_web_back_forward_list_new_with_web_view(web_view: PWebKitWebView): PWebKitWebBackForwardList; cdecl; external;
function webkit_web_data_source_get_data(AWebDataSource: PWebKitWebDataSource): PGString; cdecl; external;
function webkit_web_data_source_get_encoding(AWebDataSource: PWebKitWebDataSource): Pgchar; cdecl; external;
function webkit_web_data_source_get_initial_request(AWebDataSource: PWebKitWebDataSource): PWebKitNetworkRequest; cdecl; external;
function webkit_web_data_source_get_main_resource(AWebDataSource: PWebKitWebDataSource): PWebKitWebResource; cdecl; external;
function webkit_web_data_source_get_request(AWebDataSource: PWebKitWebDataSource): PWebKitNetworkRequest; cdecl; external;
function webkit_web_data_source_get_subresources(AWebDataSource: PWebKitWebDataSource): PGList; cdecl; external;
function webkit_web_data_source_get_type: TGType; cdecl; external;
function webkit_web_data_source_get_unreachable_uri(AWebDataSource: PWebKitWebDataSource): Pgchar; cdecl; external;
function webkit_web_data_source_get_web_frame(AWebDataSource: PWebKitWebDataSource): PWebKitWebFrame; cdecl; external;
function webkit_web_data_source_is_loading(AWebDataSource: PWebKitWebDataSource): gboolean; cdecl; external;
function webkit_web_data_source_new: PWebKitWebDataSource; cdecl; external;
function webkit_web_data_source_new_with_request(request: PWebKitNetworkRequest): PWebKitWebDataSource; cdecl; external;
function webkit_web_database_get_display_name(AWebDatabase: PWebKitWebDatabase): Pgchar; cdecl; external;
function webkit_web_database_get_expected_size(AWebDatabase: PWebKitWebDatabase): guint64; cdecl; external;
function webkit_web_database_get_filename(AWebDatabase: PWebKitWebDatabase): Pgchar; cdecl; external;
function webkit_web_database_get_name(AWebDatabase: PWebKitWebDatabase): Pgchar; cdecl; external;
function webkit_web_database_get_security_origin(AWebDatabase: PWebKitWebDatabase): PWebKitSecurityOrigin; cdecl; external;
function webkit_web_database_get_size(AWebDatabase: PWebKitWebDatabase): guint64; cdecl; external;
function webkit_web_database_get_type: TGType; cdecl; external;
function webkit_web_frame_find_frame(AWebFrame: PWebKitWebFrame; name: Pgchar): PWebKitWebFrame; cdecl; external;
function webkit_web_frame_get_data_source(AWebFrame: PWebKitWebFrame): PWebKitWebDataSource; cdecl; external;
function webkit_web_frame_get_global_context(AWebFrame: PWebKitWebFrame): TJSGlobalContextRef; cdecl; external;
function webkit_web_frame_get_horizontal_scrollbar_policy(AWebFrame: PWebKitWebFrame): TGtkPolicyType; cdecl; external;
function webkit_web_frame_get_load_status(AWebFrame: PWebKitWebFrame): TWebKitLoadStatus; cdecl; external;
function webkit_web_frame_get_name(AWebFrame: PWebKitWebFrame): Pgchar; cdecl; external;
function webkit_web_frame_get_network_response(AWebFrame: PWebKitWebFrame): PWebKitNetworkResponse; cdecl; external;
function webkit_web_frame_get_parent(AWebFrame: PWebKitWebFrame): PWebKitWebFrame; cdecl; external;
function webkit_web_frame_get_provisional_data_source(AWebFrame: PWebKitWebFrame): PWebKitWebDataSource; cdecl; external;
function webkit_web_frame_get_security_origin(AWebFrame: PWebKitWebFrame): PWebKitSecurityOrigin; cdecl; external;
function webkit_web_frame_get_title(AWebFrame: PWebKitWebFrame): Pgchar; cdecl; external;
function webkit_web_frame_get_type: TGType; cdecl; external;
function webkit_web_frame_get_uri(AWebFrame: PWebKitWebFrame): Pgchar; cdecl; external;
function webkit_web_frame_get_vertical_scrollbar_policy(AWebFrame: PWebKitWebFrame): TGtkPolicyType; cdecl; external;
function webkit_web_frame_get_web_view(AWebFrame: PWebKitWebFrame): PWebKitWebView; cdecl; external;
function webkit_web_frame_new(web_view: PWebKitWebView): PWebKitWebFrame; cdecl; external;
function webkit_web_frame_print_full(AWebFrame: PWebKitWebFrame; operation: PGtkPrintOperation; action: TGtkPrintOperationAction): TGtkPrintOperationResult; cdecl; external;
function webkit_web_history_item_copy(AWebHistoryItem: PWebKitWebHistoryItem): PWebKitWebHistoryItem; cdecl; external;
function webkit_web_history_item_get_alternate_title(AWebHistoryItem: PWebKitWebHistoryItem): Pgchar; cdecl; external;
function webkit_web_history_item_get_last_visited_time(AWebHistoryItem: PWebKitWebHistoryItem): gdouble; cdecl; external;
function webkit_web_history_item_get_original_uri(AWebHistoryItem: PWebKitWebHistoryItem): Pgchar; cdecl; external;
function webkit_web_history_item_get_title(AWebHistoryItem: PWebKitWebHistoryItem): Pgchar; cdecl; external;
function webkit_web_history_item_get_type: TGType; cdecl; external;
function webkit_web_history_item_get_uri(AWebHistoryItem: PWebKitWebHistoryItem): Pgchar; cdecl; external;
function webkit_web_history_item_new: PWebKitWebHistoryItem; cdecl; external;
function webkit_web_history_item_new_with_data(uri: Pgchar; title: Pgchar): PWebKitWebHistoryItem; cdecl; external;
function webkit_web_inspector_get_inspected_uri(AWebInspector: PWebKitWebInspector): Pgchar; cdecl; external;
function webkit_web_inspector_get_type: TGType; cdecl; external;
function webkit_web_inspector_get_web_view(AWebInspector: PWebKitWebInspector): PWebKitWebView; cdecl; external;
function webkit_web_navigation_action_get_button(AWebNavigationAction: PWebKitWebNavigationAction): gint; cdecl; external;
function webkit_web_navigation_action_get_modifier_state(AWebNavigationAction: PWebKitWebNavigationAction): gint; cdecl; external;
function webkit_web_navigation_action_get_original_uri(AWebNavigationAction: PWebKitWebNavigationAction): Pgchar; cdecl; external;
function webkit_web_navigation_action_get_reason(AWebNavigationAction: PWebKitWebNavigationAction): TWebKitWebNavigationReason; cdecl; external;
function webkit_web_navigation_action_get_target_frame(AWebNavigationAction: PWebKitWebNavigationAction): Pgchar; cdecl; external;
function webkit_web_navigation_action_get_type: TGType; cdecl; external;
function webkit_web_plugin_database_get_plugin_for_mimetype(AWebPluginDatabase: PWebKitWebPluginDatabase; param0: Pgchar): PWebKitWebPlugin; cdecl; external;
function webkit_web_plugin_database_get_plugins(AWebPluginDatabase: PWebKitWebPluginDatabase): PGSList; cdecl; external;
function webkit_web_plugin_database_get_type: TGType; cdecl; external;
function webkit_web_plugin_get_description(AWebPlugin: PWebKitWebPlugin): Pgchar; cdecl; external;
function webkit_web_plugin_get_enabled(AWebPlugin: PWebKitWebPlugin): gboolean; cdecl; external;
function webkit_web_plugin_get_mimetypes(AWebPlugin: PWebKitWebPlugin): PGSList; cdecl; external;
function webkit_web_plugin_get_name(AWebPlugin: PWebKitWebPlugin): Pgchar; cdecl; external;
function webkit_web_plugin_get_path(AWebPlugin: PWebKitWebPlugin): Pgchar; cdecl; external;
function webkit_web_plugin_get_type: TGType; cdecl; external;
function webkit_web_policy_decision_get_type: TGType; cdecl; external;
function webkit_web_resource_get_data(AWebResource: PWebKitWebResource): PGString; cdecl; external;
function webkit_web_resource_get_encoding(AWebResource: PWebKitWebResource): Pgchar; cdecl; external;
function webkit_web_resource_get_frame_name(AWebResource: PWebKitWebResource): Pgchar; cdecl; external;
function webkit_web_resource_get_mime_type(AWebResource: PWebKitWebResource): Pgchar; cdecl; external;
function webkit_web_resource_get_type: TGType; cdecl; external;
function webkit_web_resource_get_uri(AWebResource: PWebKitWebResource): Pgchar; cdecl; external;
function webkit_web_resource_new(data: Pgchar; size: gssize; uri: Pgchar; mime_type: Pgchar; encoding: Pgchar; frame_name: Pgchar): PWebKitWebResource; cdecl; external;
function webkit_web_settings_copy(AWebSettings: PWebKitWebSettings): PWebKitWebSettings; cdecl; external;
function webkit_web_settings_get_type: TGType; cdecl; external;
function webkit_web_settings_get_user_agent(AWebSettings: PWebKitWebSettings): Pgchar; cdecl; external;
function webkit_web_settings_new: PWebKitWebSettings; cdecl; external;
function webkit_web_view_can_copy_clipboard(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_can_cut_clipboard(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_can_go_back(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_can_go_back_or_forward(AWebView: PWebKitWebView; steps: gint): gboolean; cdecl; external;
function webkit_web_view_can_go_forward(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_can_paste_clipboard(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_can_redo(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_can_show_mime_type(AWebView: PWebKitWebView; mime_type: Pgchar): gboolean; cdecl; external;
function webkit_web_view_can_undo(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_get_back_forward_list(AWebView: PWebKitWebView): PWebKitWebBackForwardList; cdecl; external;
function webkit_web_view_get_copy_target_list(AWebView: PWebKitWebView): PGtkTargetList; cdecl; external;
function webkit_web_view_get_custom_encoding(AWebView: PWebKitWebView): Pgchar; cdecl; external;
function webkit_web_view_get_dom_document(AWebView: PWebKitWebView): PWebKitDOMDocument; cdecl; external;
function webkit_web_view_get_editable(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_get_encoding(AWebView: PWebKitWebView): Pgchar; cdecl; external;
function webkit_web_view_get_focused_frame(AWebView: PWebKitWebView): PWebKitWebFrame; cdecl; external;
function webkit_web_view_get_full_content_zoom(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_get_hit_test_result(AWebView: PWebKitWebView; event: PGdkEventButton): PWebKitHitTestResult; cdecl; external;
function webkit_web_view_get_icon_pixbuf(AWebView: PWebKitWebView): PGdkPixbuf; cdecl; external;
function webkit_web_view_get_icon_uri(AWebView: PWebKitWebView): Pgchar; cdecl; external;
function webkit_web_view_get_inspector(AWebView: PWebKitWebView): PWebKitWebInspector; cdecl; external;
function webkit_web_view_get_load_status(AWebView: PWebKitWebView): TWebKitLoadStatus; cdecl; external;
function webkit_web_view_get_main_frame(AWebView: PWebKitWebView): PWebKitWebFrame; cdecl; external;
function webkit_web_view_get_paste_target_list(AWebView: PWebKitWebView): PGtkTargetList; cdecl; external;
function webkit_web_view_get_progress(AWebView: PWebKitWebView): gdouble; cdecl; external;
function webkit_web_view_get_settings(AWebView: PWebKitWebView): PWebKitWebSettings; cdecl; external;
function webkit_web_view_get_title(AWebView: PWebKitWebView): Pgchar; cdecl; external;
function webkit_web_view_get_transparent(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_get_type: TGType; cdecl; external;
function webkit_web_view_get_uri(AWebView: PWebKitWebView): Pgchar; cdecl; external;
function webkit_web_view_get_view_mode(AWebView: PWebKitWebView): TWebKitWebViewViewMode; cdecl; external;
function webkit_web_view_get_view_source_mode(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_get_viewport_attributes(AWebView: PWebKitWebView): PWebKitViewportAttributes; cdecl; external;
function webkit_web_view_get_window_features(AWebView: PWebKitWebView): PWebKitWebWindowFeatures; cdecl; external;
function webkit_web_view_get_zoom_level(AWebView: PWebKitWebView): gfloat; cdecl; external;
function webkit_web_view_go_to_back_forward_item(AWebView: PWebKitWebView; item: PWebKitWebHistoryItem): gboolean; cdecl; external;
function webkit_web_view_has_selection(AWebView: PWebKitWebView): gboolean; cdecl; external;
function webkit_web_view_mark_text_matches(AWebView: PWebKitWebView; string_: Pgchar; case_sensitive: gboolean; limit: guint): guint; cdecl; external;
function webkit_web_view_new: PWebKitWebView; cdecl; external;
function webkit_web_view_search_text(AWebView: PWebKitWebView; text: Pgchar; case_sensitive: gboolean; forward: gboolean; wrap: gboolean): gboolean; cdecl; external;
function webkit_web_window_features_equal(AWebWindowFeatures: PWebKitWebWindowFeatures; features2: PWebKitWebWindowFeatures): gboolean; cdecl; external;
function webkit_web_window_features_get_type: TGType; cdecl; external;
function webkit_web_window_features_new: PWebKitWebWindowFeatures; cdecl; external;
procedure webkit_application_cache_set_maximum_size(size: unsigned_long_long); cdecl; external;
procedure webkit_dom_attr_set_value(ADOMAttr: PWebKitDOMAttr; value: Pgchar); cdecl; external;
procedure webkit_dom_character_data_append_data(ADOMCharacterData: PWebKitDOMCharacterData; data: Pgchar); cdecl; external;
procedure webkit_dom_character_data_delete_data(ADOMCharacterData: PWebKitDOMCharacterData; offset: gulong; length: gulong); cdecl; external;
procedure webkit_dom_character_data_insert_data(ADOMCharacterData: PWebKitDOMCharacterData; offset: gulong; data: Pgchar); cdecl; external;
procedure webkit_dom_character_data_replace_data(ADOMCharacterData: PWebKitDOMCharacterData; offset: gulong; length: gulong; data: Pgchar); cdecl; external;
procedure webkit_dom_character_data_set_data(ADOMCharacterData: PWebKitDOMCharacterData; value: Pgchar); cdecl; external;
procedure webkit_dom_console_group_end(ADOMConsole: PWebKitDOMConsole); cdecl; external;
procedure webkit_dom_console_time(ADOMConsole: PWebKitDOMConsole; title: Pgchar); cdecl; external;
procedure webkit_dom_css_rule_set_css_text(ADOMCSSRule: PWebKitDOMCSSRule; value: Pgchar); cdecl; external;
procedure webkit_dom_css_style_declaration_set_css_text(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; value: Pgchar); cdecl; external;
procedure webkit_dom_css_style_declaration_set_property(ADOMCSSStyleDeclaration: PWebKitDOMCSSStyleDeclaration; property_name: Pgchar; value: Pgchar; priority: Pgchar); cdecl; external;
procedure webkit_dom_css_style_sheet_delete_rule(ADOMCSSStyleSheet: PWebKitDOMCSSStyleSheet; index: gulong); cdecl; external;
procedure webkit_dom_css_style_sheet_remove_rule(ADOMCSSStyleSheet: PWebKitDOMCSSStyleSheet; index: gulong); cdecl; external;
procedure webkit_dom_css_value_set_css_text(ADOMCSSValue: PWebKitDOMCSSValue; value: Pgchar); cdecl; external;
procedure webkit_dom_document_set_body(ADOMDocument: PWebKitDOMDocument; value: PWebKitDOMHTMLElement); cdecl; external;
procedure webkit_dom_document_set_charset(ADOMDocument: PWebKitDOMDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_document_set_cookie(ADOMDocument: PWebKitDOMDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_document_set_document_uri(ADOMDocument: PWebKitDOMDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_document_set_selected_stylesheet_set(ADOMDocument: PWebKitDOMDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_document_set_title(ADOMDocument: PWebKitDOMDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_document_set_xml_standalone(ADOMDocument: PWebKitDOMDocument; value: gboolean); cdecl; external;
procedure webkit_dom_document_set_xml_version(ADOMDocument: PWebKitDOMDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_document_webkit_cancel_full_screen(ADOMDocument: PWebKitDOMDocument); cdecl; external;
procedure webkit_dom_dom_application_cache_swap_cache(ADOMDOMApplicationCache: PWebKitDOMDOMApplicationCache); cdecl; external;
procedure webkit_dom_dom_application_cache_update(ADOMDOMApplicationCache: PWebKitDOMDOMApplicationCache); cdecl; external;
procedure webkit_dom_dom_plugin_array_refresh(ADOMDOMPluginArray: PWebKitDOMDOMPluginArray; reload: gboolean); cdecl; external;
procedure webkit_dom_dom_selection_add_range(ADOMDOMSelection: PWebKitDOMDOMSelection; range: PWebKitDOMRange); cdecl; external;
procedure webkit_dom_dom_selection_collapse(ADOMDOMSelection: PWebKitDOMDOMSelection; node: PWebKitDOMNode; index: glong); cdecl; external;
procedure webkit_dom_dom_selection_collapse_to_end(ADOMDOMSelection: PWebKitDOMDOMSelection); cdecl; external;
procedure webkit_dom_dom_selection_collapse_to_start(ADOMDOMSelection: PWebKitDOMDOMSelection); cdecl; external;
procedure webkit_dom_dom_selection_delete_from_document(ADOMDOMSelection: PWebKitDOMDOMSelection); cdecl; external;
procedure webkit_dom_dom_selection_empty(ADOMDOMSelection: PWebKitDOMDOMSelection); cdecl; external;
procedure webkit_dom_dom_selection_extend(ADOMDOMSelection: PWebKitDOMDOMSelection; node: PWebKitDOMNode; offset: glong); cdecl; external;
procedure webkit_dom_dom_selection_modify(ADOMDOMSelection: PWebKitDOMDOMSelection; alter: Pgchar; direction: Pgchar; granularity: Pgchar); cdecl; external;
procedure webkit_dom_dom_selection_remove_all_ranges(ADOMDOMSelection: PWebKitDOMDOMSelection); cdecl; external;
procedure webkit_dom_dom_selection_select_all_children(ADOMDOMSelection: PWebKitDOMDOMSelection; node: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_dom_selection_set_base_and_extent(ADOMDOMSelection: PWebKitDOMDOMSelection; base_node: PWebKitDOMNode; base_offset: glong; extent_node: PWebKitDOMNode; extent_offset: glong); cdecl; external;
procedure webkit_dom_dom_selection_set_position(ADOMDOMSelection: PWebKitDOMDOMSelection; node: PWebKitDOMNode; offset: glong); cdecl; external;
procedure webkit_dom_dom_settable_token_list_set_value(ADOMDOMSettableTokenList: PWebKitDOMDOMSettableTokenList; value: Pgchar); cdecl; external;
procedure webkit_dom_dom_token_list_add(ADOMDOMTokenList: PWebKitDOMDOMTokenList; token: Pgchar); cdecl; external;
procedure webkit_dom_dom_token_list_remove(ADOMDOMTokenList: PWebKitDOMDOMTokenList; token: Pgchar); cdecl; external;
procedure webkit_dom_dom_window_alert(ADOMDOMWindow: PWebKitDOMDOMWindow; message: Pgchar); cdecl; external;
procedure webkit_dom_dom_window_blur(ADOMDOMWindow: PWebKitDOMDOMWindow); cdecl; external;
procedure webkit_dom_dom_window_capture_events(ADOMDOMWindow: PWebKitDOMDOMWindow); cdecl; external;
procedure webkit_dom_dom_window_clear_interval(ADOMDOMWindow: PWebKitDOMDOMWindow; handle: glong); cdecl; external;
procedure webkit_dom_dom_window_clear_timeout(ADOMDOMWindow: PWebKitDOMDOMWindow; handle: glong); cdecl; external;
procedure webkit_dom_dom_window_close(ADOMDOMWindow: PWebKitDOMDOMWindow); cdecl; external;
procedure webkit_dom_dom_window_focus(ADOMDOMWindow: PWebKitDOMDOMWindow); cdecl; external;
procedure webkit_dom_dom_window_move_by(ADOMDOMWindow: PWebKitDOMDOMWindow; x: gfloat; y: gfloat); cdecl; external;
procedure webkit_dom_dom_window_move_to(ADOMDOMWindow: PWebKitDOMDOMWindow; x: gfloat; y: gfloat); cdecl; external;
procedure webkit_dom_dom_window_print(ADOMDOMWindow: PWebKitDOMDOMWindow); cdecl; external;
procedure webkit_dom_dom_window_release_events(ADOMDOMWindow: PWebKitDOMDOMWindow); cdecl; external;
procedure webkit_dom_dom_window_resize_by(ADOMDOMWindow: PWebKitDOMDOMWindow; x: gfloat; y: gfloat); cdecl; external;
procedure webkit_dom_dom_window_resize_to(ADOMDOMWindow: PWebKitDOMDOMWindow; width: gfloat; height: gfloat); cdecl; external;
procedure webkit_dom_dom_window_scroll(ADOMDOMWindow: PWebKitDOMDOMWindow; x: glong; y: glong); cdecl; external;
procedure webkit_dom_dom_window_scroll_by(ADOMDOMWindow: PWebKitDOMDOMWindow; x: glong; y: glong); cdecl; external;
procedure webkit_dom_dom_window_scroll_to(ADOMDOMWindow: PWebKitDOMDOMWindow; x: glong; y: glong); cdecl; external;
procedure webkit_dom_dom_window_set_default_status(ADOMDOMWindow: PWebKitDOMDOMWindow; value: Pgchar); cdecl; external;
procedure webkit_dom_dom_window_set_name(ADOMDOMWindow: PWebKitDOMDOMWindow; value: Pgchar); cdecl; external;
procedure webkit_dom_dom_window_set_status(ADOMDOMWindow: PWebKitDOMDOMWindow; value: Pgchar); cdecl; external;
procedure webkit_dom_dom_window_stop(ADOMDOMWindow: PWebKitDOMDOMWindow); cdecl; external;
procedure webkit_dom_element_blur(ADOMElement: PWebKitDOMElement); cdecl; external;
procedure webkit_dom_element_focus(ADOMElement: PWebKitDOMElement); cdecl; external;
procedure webkit_dom_element_remove_attribute(ADOMElement: PWebKitDOMElement; name: Pgchar); cdecl; external;
procedure webkit_dom_element_remove_attribute_ns(ADOMElement: PWebKitDOMElement; namespace_uri: Pgchar; local_name: Pgchar); cdecl; external;
procedure webkit_dom_element_scroll_by_lines(ADOMElement: PWebKitDOMElement; lines: glong); cdecl; external;
procedure webkit_dom_element_scroll_by_pages(ADOMElement: PWebKitDOMElement; pages: glong); cdecl; external;
procedure webkit_dom_element_scroll_into_view(ADOMElement: PWebKitDOMElement; align_with_top: gboolean); cdecl; external;
procedure webkit_dom_element_scroll_into_view_if_needed(ADOMElement: PWebKitDOMElement; center_if_needed: gboolean); cdecl; external;
procedure webkit_dom_element_set_attribute(ADOMElement: PWebKitDOMElement; name: Pgchar; value: Pgchar); cdecl; external;
procedure webkit_dom_element_set_attribute_ns(ADOMElement: PWebKitDOMElement; namespace_uri: Pgchar; qualified_name: Pgchar; value: Pgchar); cdecl; external;
procedure webkit_dom_element_set_scroll_left(ADOMElement: PWebKitDOMElement; value: glong); cdecl; external;
procedure webkit_dom_element_set_scroll_top(ADOMElement: PWebKitDOMElement; value: glong); cdecl; external;
procedure webkit_dom_element_webkit_request_full_screen(ADOMElement: PWebKitDOMElement; flags: gushort); cdecl; external;
procedure webkit_dom_event_init_event(ADOMEvent: PWebKitDOMEvent; event_type_arg: Pgchar; can_bubble_arg: gboolean; cancelable_arg: gboolean); cdecl; external;
procedure webkit_dom_event_prevent_default(ADOMEvent: PWebKitDOMEvent); cdecl; external;
procedure webkit_dom_event_set_cancel_bubble(ADOMEvent: PWebKitDOMEvent; value: gboolean); cdecl; external;
procedure webkit_dom_event_set_return_value(ADOMEvent: PWebKitDOMEvent; value: gboolean); cdecl; external;
procedure webkit_dom_event_stop_immediate_propagation(ADOMEvent: PWebKitDOMEvent); cdecl; external;
procedure webkit_dom_event_stop_propagation(ADOMEvent: PWebKitDOMEvent); cdecl; external;
procedure webkit_dom_event_target_dispatch_event(ADOMEventTarget: PWebKitDOMEventTarget; event: PWebKitDOMEvent); cdecl; external;
procedure webkit_dom_history_back(ADOMHistory: PWebKitDOMHistory); cdecl; external;
procedure webkit_dom_history_forward(ADOMHistory: PWebKitDOMHistory); cdecl; external;
procedure webkit_dom_history_go(ADOMHistory: PWebKitDOMHistory; distance: glong); cdecl; external;
procedure webkit_dom_html_anchor_element_set_access_key(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_charset(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_coords(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_hash(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_host(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_hostname(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_href(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_hreflang(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_name(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_pathname(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_port(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_protocol(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_rel(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_rev(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_search(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_shape(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_anchor_element_set_target(ADOMHTMLAnchorElement: PWebKitDOMHTMLAnchorElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_align(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_alt(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_archive(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_code(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_code_base(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_height(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_hspace(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: glong); cdecl; external;
procedure webkit_dom_html_applet_element_set_name(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_object(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_applet_element_set_vspace(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: glong); cdecl; external;
procedure webkit_dom_html_applet_element_set_width(ADOMHTMLAppletElement: PWebKitDOMHTMLAppletElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_area_element_set_access_key(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_area_element_set_alt(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_area_element_set_coords(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_area_element_set_href(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_area_element_set_no_href(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_area_element_set_shape(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_area_element_set_target(ADOMHTMLAreaElement: PWebKitDOMHTMLAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_base_element_set_href(ADOMHTMLBaseElement: PWebKitDOMHTMLBaseElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_base_element_set_target(ADOMHTMLBaseElement: PWebKitDOMHTMLBaseElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_base_font_element_set_color(ADOMHTMLBaseFontElement: PWebKitDOMHTMLBaseFontElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_base_font_element_set_face(ADOMHTMLBaseFontElement: PWebKitDOMHTMLBaseFontElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_base_font_element_set_size(ADOMHTMLBaseFontElement: PWebKitDOMHTMLBaseFontElement; value: glong); cdecl; external;
procedure webkit_dom_html_blockquote_element_set_cite(ADOMHTMLBlockquoteElement: PWebKitDOMHTMLBlockquoteElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_body_element_set_a_link(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_body_element_set_background(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_body_element_set_bg_color(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_body_element_set_link(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_body_element_set_text(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_body_element_set_v_link(ADOMHTMLBodyElement: PWebKitDOMHTMLBodyElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_button_element_click(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement); cdecl; external;
procedure webkit_dom_html_button_element_set_access_key(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_button_element_set_autofocus(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_button_element_set_custom_validity(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; error: Pgchar); cdecl; external;
procedure webkit_dom_html_button_element_set_disabled(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_button_element_set_form_action(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_button_element_set_form_enctype(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_button_element_set_form_method(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_button_element_set_form_no_validate(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_button_element_set_form_target(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_button_element_set_name(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_button_element_set_value(ADOMHTMLButtonElement: PWebKitDOMHTMLButtonElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_canvas_element_set_height(ADOMHTMLCanvasElement: PWebKitDOMHTMLCanvasElement; value: glong); cdecl; external;
procedure webkit_dom_html_canvas_element_set_width(ADOMHTMLCanvasElement: PWebKitDOMHTMLCanvasElement; value: glong); cdecl; external;
procedure webkit_dom_html_details_element_set_open(ADOMHTMLDetailsElement: PWebKitDOMHTMLDetailsElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_directory_element_set_compact(ADOMHTMLDirectoryElement: PWebKitDOMHTMLDirectoryElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_div_element_set_align(ADOMHTMLDivElement: PWebKitDOMHTMLDivElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_document_capture_events(ADOMHTMLDocument: PWebKitDOMHTMLDocument); cdecl; external;
procedure webkit_dom_html_document_clear(ADOMHTMLDocument: PWebKitDOMHTMLDocument); cdecl; external;
procedure webkit_dom_html_document_close(ADOMHTMLDocument: PWebKitDOMHTMLDocument); cdecl; external;
procedure webkit_dom_html_document_release_events(ADOMHTMLDocument: PWebKitDOMHTMLDocument); cdecl; external;
procedure webkit_dom_html_document_set_alink_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_html_document_set_bg_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_html_document_set_design_mode(ADOMHTMLDocument: PWebKitDOMHTMLDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_html_document_set_dir(ADOMHTMLDocument: PWebKitDOMHTMLDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_html_document_set_fg_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_html_document_set_link_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_html_document_set_vlink_color(ADOMHTMLDocument: PWebKitDOMHTMLDocument; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_insert_adjacent_html(ADOMHTMLElement: PWebKitDOMHTMLElement; where: Pgchar; html: Pgchar); cdecl; external;
procedure webkit_dom_html_element_insert_adjacent_text(ADOMHTMLElement: PWebKitDOMHTMLElement; where: Pgchar; text: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_class_name(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_content_editable(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_dir(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_draggable(ADOMHTMLElement: PWebKitDOMHTMLElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_element_set_hidden(ADOMHTMLElement: PWebKitDOMHTMLElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_element_set_id(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_inner_html(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_inner_text(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_lang(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_outer_html(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_outer_text(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_element_set_spellcheck(ADOMHTMLElement: PWebKitDOMHTMLElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_element_set_tab_index(ADOMHTMLElement: PWebKitDOMHTMLElement; value: glong); cdecl; external;
procedure webkit_dom_html_element_set_title(ADOMHTMLElement: PWebKitDOMHTMLElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_embed_element_set_align(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_embed_element_set_height(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement; value: glong); cdecl; external;
procedure webkit_dom_html_embed_element_set_name(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_embed_element_set_src(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_embed_element_set_width(ADOMHTMLEmbedElement: PWebKitDOMHTMLEmbedElement; value: glong); cdecl; external;
procedure webkit_dom_html_field_set_element_set_custom_validity(ADOMHTMLFieldSetElement: PWebKitDOMHTMLFieldSetElement; error: Pgchar); cdecl; external;
procedure webkit_dom_html_font_element_set_color(ADOMHTMLFontElement: PWebKitDOMHTMLFontElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_font_element_set_face(ADOMHTMLFontElement: PWebKitDOMHTMLFontElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_font_element_set_size(ADOMHTMLFontElement: PWebKitDOMHTMLFontElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_form_element_dispatch_form_change(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement); cdecl; external;
procedure webkit_dom_html_form_element_dispatch_form_input(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement); cdecl; external;
procedure webkit_dom_html_form_element_reset(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement); cdecl; external;
procedure webkit_dom_html_form_element_set_accept_charset(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_form_element_set_action(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_form_element_set_encoding(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_form_element_set_enctype(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_form_element_set_method(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_form_element_set_name(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_form_element_set_no_validate(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_form_element_set_target(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_form_element_submit(ADOMHTMLFormElement: PWebKitDOMHTMLFormElement); cdecl; external;
procedure webkit_dom_html_frame_element_set_frame_border(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_frame_element_set_long_desc(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_frame_element_set_margin_height(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_frame_element_set_margin_width(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_frame_element_set_name(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_frame_element_set_no_resize(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_frame_element_set_scrolling(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_frame_element_set_src(ADOMHTMLFrameElement: PWebKitDOMHTMLFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_frame_set_element_set_cols(ADOMHTMLFrameSetElement: PWebKitDOMHTMLFrameSetElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_frame_set_element_set_rows(ADOMHTMLFrameSetElement: PWebKitDOMHTMLFrameSetElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_head_element_set_profile(ADOMHTMLHeadElement: PWebKitDOMHTMLHeadElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_heading_element_set_align(ADOMHTMLHeadingElement: PWebKitDOMHTMLHeadingElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_html_element_set_manifest(ADOMHTMLHtmlElement: PWebKitDOMHTMLHtmlElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_html_element_set_version(ADOMHTMLHtmlElement: PWebKitDOMHTMLHtmlElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_align(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_frame_border(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_height(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_long_desc(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_margin_height(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_margin_width(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_name(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_sandbox(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_scrolling(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_src(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_iframe_element_set_width(ADOMHTMLIFrameElement: PWebKitDOMHTMLIFrameElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_align(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_alt(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_border(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_height(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: glong); cdecl; external;
procedure webkit_dom_html_image_element_set_hspace(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: glong); cdecl; external;
procedure webkit_dom_html_image_element_set_is_map(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_image_element_set_long_desc(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_lowsrc(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_name(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_src(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_use_map(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_image_element_set_vspace(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: glong); cdecl; external;
procedure webkit_dom_html_image_element_set_width(ADOMHTMLImageElement: PWebKitDOMHTMLImageElement; value: glong); cdecl; external;
procedure webkit_dom_html_input_element_click(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement); cdecl; external;
procedure webkit_dom_html_input_element_select(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement); cdecl; external;
procedure webkit_dom_html_input_element_set_accept(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_access_key(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_align(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_alt(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_autofocus(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_checked(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_custom_validity(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; error: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_default_checked(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_default_value(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_disabled(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_form_action(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_form_enctype(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_form_method(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_form_no_validate(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_form_target(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_incremental(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_indeterminate(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_max(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_max_length(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: glong); cdecl; external;
procedure webkit_dom_html_input_element_set_min(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_multiple(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_name(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_pattern(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_placeholder(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_read_only(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_required(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_input_element_set_size(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gulong); cdecl; external;
procedure webkit_dom_html_input_element_set_src(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_step(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_use_map(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_value(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_set_value_as_number(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: gdouble); cdecl; external;
procedure webkit_dom_html_input_element_set_value_for_user(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_input_element_step_down(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; n: glong); cdecl; external;
procedure webkit_dom_html_input_element_step_up(ADOMHTMLInputElement: PWebKitDOMHTMLInputElement; n: glong); cdecl; external;
procedure webkit_dom_html_is_index_element_set_prompt(ADOMHTMLIsIndexElement: PWebKitDOMHTMLIsIndexElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_keygen_element_set_autofocus(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_keygen_element_set_challenge(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_keygen_element_set_custom_validity(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement; error: Pgchar); cdecl; external;
procedure webkit_dom_html_keygen_element_set_disabled(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_keygen_element_set_keytype(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_keygen_element_set_name(ADOMHTMLKeygenElement: PWebKitDOMHTMLKeygenElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_label_element_set_access_key(ADOMHTMLLabelElement: PWebKitDOMHTMLLabelElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_label_element_set_html_for(ADOMHTMLLabelElement: PWebKitDOMHTMLLabelElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_legend_element_set_access_key(ADOMHTMLLegendElement: PWebKitDOMHTMLLegendElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_legend_element_set_align(ADOMHTMLLegendElement: PWebKitDOMHTMLLegendElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_link_element_set_charset(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_link_element_set_disabled(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_link_element_set_href(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_link_element_set_hreflang(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_link_element_set_media(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_link_element_set_rel(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_link_element_set_rev(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_link_element_set_target(ADOMHTMLLinkElement: PWebKitDOMHTMLLinkElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_map_element_set_name(ADOMHTMLMapElement: PWebKitDOMHTMLMapElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_marquee_element_set_behavior(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_marquee_element_set_bg_color(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_marquee_element_set_direction(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_marquee_element_set_height(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_marquee_element_set_hspace(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: gulong); cdecl; external;
procedure webkit_dom_html_marquee_element_set_loop(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: glong); cdecl; external;
procedure webkit_dom_html_marquee_element_set_scroll_amount(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: glong); cdecl; external;
procedure webkit_dom_html_marquee_element_set_scroll_delay(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: glong); cdecl; external;
procedure webkit_dom_html_marquee_element_set_true_speed(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_marquee_element_set_vspace(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: gulong); cdecl; external;
procedure webkit_dom_html_marquee_element_set_width(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_marquee_element_start(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement); cdecl; external;
procedure webkit_dom_html_marquee_element_stop(ADOMHTMLMarqueeElement: PWebKitDOMHTMLMarqueeElement); cdecl; external;
procedure webkit_dom_html_media_element_load(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; isUserGesture: gboolean); cdecl; external;
procedure webkit_dom_html_media_element_pause(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; isUserGesture: gboolean); cdecl; external;
procedure webkit_dom_html_media_element_play(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; isUserGesture: gboolean); cdecl; external;
procedure webkit_dom_html_media_element_set_autoplay(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_media_element_set_controls(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_media_element_set_current_time(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gfloat); cdecl; external;
procedure webkit_dom_html_media_element_set_default_playback_rate(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gfloat); cdecl; external;
procedure webkit_dom_html_media_element_set_loop(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_media_element_set_muted(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_media_element_set_playback_rate(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gfloat); cdecl; external;
procedure webkit_dom_html_media_element_set_preload(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_media_element_set_src(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_media_element_set_volume(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gfloat); cdecl; external;
procedure webkit_dom_html_media_element_set_webkit_closed_captions_visible(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_media_element_set_webkit_preserves_pitch(ADOMHTMLMediaElement: PWebKitDOMHTMLMediaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_menu_element_set_compact(ADOMHTMLMenuElement: PWebKitDOMHTMLMenuElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_meta_element_set_content(ADOMHTMLMetaElement: PWebKitDOMHTMLMetaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_meta_element_set_http_equiv(ADOMHTMLMetaElement: PWebKitDOMHTMLMetaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_meta_element_set_name(ADOMHTMLMetaElement: PWebKitDOMHTMLMetaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_meta_element_set_scheme(ADOMHTMLMetaElement: PWebKitDOMHTMLMetaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_mod_element_set_cite(ADOMHTMLModElement: PWebKitDOMHTMLModElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_mod_element_set_date_time(ADOMHTMLModElement: PWebKitDOMHTMLModElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_align(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_archive(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_border(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_code(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_code_base(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_code_type(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_custom_validity(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; error: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_data(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_declare(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_object_element_set_height(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_hspace(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: glong); cdecl; external;
procedure webkit_dom_html_object_element_set_name(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_standby(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_use_map(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_object_element_set_vspace(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: glong); cdecl; external;
procedure webkit_dom_html_object_element_set_width(ADOMHTMLObjectElement: PWebKitDOMHTMLObjectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_opt_group_element_set_disabled(ADOMHTMLOptGroupElement: PWebKitDOMHTMLOptGroupElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_opt_group_element_set_label(ADOMHTMLOptGroupElement: PWebKitDOMHTMLOptGroupElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_option_element_set_default_selected(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_option_element_set_disabled(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_option_element_set_label(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_option_element_set_selected(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_option_element_set_value(ADOMHTMLOptionElement: PWebKitDOMHTMLOptionElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_options_collection_set_selected_index(ADOMHTMLOptionsCollection: PWebKitDOMHTMLOptionsCollection; value: glong); cdecl; external;
procedure webkit_dom_html_paragraph_element_set_align(ADOMHTMLParagraphElement: PWebKitDOMHTMLParagraphElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_param_element_set_name(ADOMHTMLParamElement: PWebKitDOMHTMLParamElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_param_element_set_value(ADOMHTMLParamElement: PWebKitDOMHTMLParamElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_param_element_set_value_type(ADOMHTMLParamElement: PWebKitDOMHTMLParamElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_pre_element_set_width(ADOMHTMLPreElement: PWebKitDOMHTMLPreElement; value: glong); cdecl; external;
procedure webkit_dom_html_pre_element_set_wrap(ADOMHTMLPreElement: PWebKitDOMHTMLPreElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_quote_element_set_cite(ADOMHTMLQuoteElement: PWebKitDOMHTMLQuoteElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_script_element_set_async(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_script_element_set_charset(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_script_element_set_defer(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_script_element_set_event(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_script_element_set_html_for(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_script_element_set_src(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_script_element_set_text(ADOMHTMLScriptElement: PWebKitDOMHTMLScriptElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_select_element_add(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; element: PWebKitDOMHTMLElement; before: PWebKitDOMHTMLElement); cdecl; external;
procedure webkit_dom_html_select_element_remove(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; index: glong); cdecl; external;
procedure webkit_dom_html_select_element_set_autofocus(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_select_element_set_custom_validity(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; error: Pgchar); cdecl; external;
procedure webkit_dom_html_select_element_set_disabled(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_select_element_set_length(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: gulong); cdecl; external;
procedure webkit_dom_html_select_element_set_multiple(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_select_element_set_name(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_select_element_set_required(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_select_element_set_selected_index(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: glong); cdecl; external;
procedure webkit_dom_html_select_element_set_size(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: glong); cdecl; external;
procedure webkit_dom_html_select_element_set_value(ADOMHTMLSelectElement: PWebKitDOMHTMLSelectElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_style_element_set_disabled(ADOMHTMLStyleElement: PWebKitDOMHTMLStyleElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_style_element_set_media(ADOMHTMLStyleElement: PWebKitDOMHTMLStyleElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_caption_element_set_align(ADOMHTMLTableCaptionElement: PWebKitDOMHTMLTableCaptionElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_abbr(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_align(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_axis(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_bg_color(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_ch(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_ch_off(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_col_span(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: glong); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_headers(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_height(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_no_wrap(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_row_span(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: glong); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_scope(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_v_align(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_cell_element_set_width(ADOMHTMLTableCellElement: PWebKitDOMHTMLTableCellElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_col_element_set_align(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_col_element_set_ch(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_col_element_set_ch_off(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_col_element_set_span(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement; value: glong); cdecl; external;
procedure webkit_dom_html_table_col_element_set_v_align(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_col_element_set_width(ADOMHTMLTableColElement: PWebKitDOMHTMLTableColElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_delete_caption(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement); cdecl; external;
procedure webkit_dom_html_table_element_delete_row(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; index: glong); cdecl; external;
procedure webkit_dom_html_table_element_delete_t_foot(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement); cdecl; external;
procedure webkit_dom_html_table_element_delete_t_head(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement); cdecl; external;
procedure webkit_dom_html_table_element_set_align(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_set_bg_color(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_set_border(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_set_caption(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: PWebKitDOMHTMLTableCaptionElement); cdecl; external;
procedure webkit_dom_html_table_element_set_cell_padding(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_set_cell_spacing(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_set_frame(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_set_rules(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_set_summary(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_element_set_t_foot(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: PWebKitDOMHTMLTableSectionElement); cdecl; external;
procedure webkit_dom_html_table_element_set_t_head(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: PWebKitDOMHTMLTableSectionElement); cdecl; external;
procedure webkit_dom_html_table_element_set_width(ADOMHTMLTableElement: PWebKitDOMHTMLTableElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_row_element_delete_cell(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement; index: glong); cdecl; external;
procedure webkit_dom_html_table_row_element_set_align(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_row_element_set_bg_color(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_row_element_set_ch(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_row_element_set_ch_off(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_row_element_set_v_align(ADOMHTMLTableRowElement: PWebKitDOMHTMLTableRowElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_section_element_delete_row(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement; index: glong); cdecl; external;
procedure webkit_dom_html_table_section_element_set_align(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_section_element_set_ch(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_section_element_set_ch_off(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_table_section_element_set_v_align(ADOMHTMLTableSectionElement: PWebKitDOMHTMLTableSectionElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_text_area_element_select(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement); cdecl; external;
procedure webkit_dom_html_text_area_element_set_access_key(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_text_area_element_set_autofocus(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_text_area_element_set_cols(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: glong); cdecl; external;
procedure webkit_dom_html_text_area_element_set_custom_validity(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; error: Pgchar); cdecl; external;
procedure webkit_dom_html_text_area_element_set_default_value(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_text_area_element_set_disabled(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_text_area_element_set_max_length(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: glong); cdecl; external;
procedure webkit_dom_html_text_area_element_set_name(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_text_area_element_set_placeholder(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_text_area_element_set_read_only(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_text_area_element_set_required(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: gboolean); cdecl; external;
procedure webkit_dom_html_text_area_element_set_rows(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: glong); cdecl; external;
procedure webkit_dom_html_text_area_element_set_selection_end(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: glong); cdecl; external;
procedure webkit_dom_html_text_area_element_set_selection_range(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; start: glong; end_: glong); cdecl; external;
procedure webkit_dom_html_text_area_element_set_selection_start(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: glong); cdecl; external;
procedure webkit_dom_html_text_area_element_set_value(ADOMHTMLTextAreaElement: PWebKitDOMHTMLTextAreaElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_title_element_set_text(ADOMHTMLTitleElement: PWebKitDOMHTMLTitleElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_video_element_set_height(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement; value: gulong); cdecl; external;
procedure webkit_dom_html_video_element_set_poster(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement; value: Pgchar); cdecl; external;
procedure webkit_dom_html_video_element_set_width(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement; value: gulong); cdecl; external;
procedure webkit_dom_html_video_element_webkit_enter_full_screen(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement; isUserGesture: gboolean); cdecl; external;
procedure webkit_dom_html_video_element_webkit_enter_fullscreen(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement; isUserGesture: gboolean); cdecl; external;
procedure webkit_dom_html_video_element_webkit_exit_full_screen(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement); cdecl; external;
procedure webkit_dom_html_video_element_webkit_exit_fullscreen(ADOMHTMLVideoElement: PWebKitDOMHTMLVideoElement); cdecl; external;
procedure webkit_dom_htmlbr_element_set_clear(ADOMHTMLBRElement: PWebKitDOMHTMLBRElement; value: Pgchar); cdecl; external;
procedure webkit_dom_htmld_list_element_set_compact(ADOMHTMLDListElement: PWebKitDOMHTMLDListElement; value: gboolean); cdecl; external;
procedure webkit_dom_htmlhr_element_set_align(ADOMHTMLHRElement: PWebKitDOMHTMLHRElement; value: Pgchar); cdecl; external;
procedure webkit_dom_htmlhr_element_set_no_shade(ADOMHTMLHRElement: PWebKitDOMHTMLHRElement; value: gboolean); cdecl; external;
procedure webkit_dom_htmlhr_element_set_size(ADOMHTMLHRElement: PWebKitDOMHTMLHRElement; value: Pgchar); cdecl; external;
procedure webkit_dom_htmlhr_element_set_width(ADOMHTMLHRElement: PWebKitDOMHTMLHRElement; value: Pgchar); cdecl; external;
procedure webkit_dom_htmlli_element_set_value(ADOMHTMLLIElement: PWebKitDOMHTMLLIElement; value: glong); cdecl; external;
procedure webkit_dom_htmlo_list_element_set_compact(ADOMHTMLOListElement: PWebKitDOMHTMLOListElement; value: gboolean); cdecl; external;
procedure webkit_dom_htmlo_list_element_set_start(ADOMHTMLOListElement: PWebKitDOMHTMLOListElement; value: glong); cdecl; external;
procedure webkit_dom_htmlu_list_element_set_compact(ADOMHTMLUListElement: PWebKitDOMHTMLUListElement; value: gboolean); cdecl; external;
procedure webkit_dom_media_list_append_medium(ADOMMediaList: PWebKitDOMMediaList; new_medium: Pgchar); cdecl; external;
procedure webkit_dom_media_list_delete_medium(ADOMMediaList: PWebKitDOMMediaList; old_medium: Pgchar); cdecl; external;
procedure webkit_dom_media_list_set_media_text(ADOMMediaList: PWebKitDOMMediaList; value: Pgchar); cdecl; external;
procedure webkit_dom_mouse_event_init_mouse_event(ADOMMouseEvent: PWebKitDOMMouseEvent; type_: Pgchar; can_bubble: gboolean; cancelable: gboolean; view: PWebKitDOMDOMWindow; detail: glong; screen_x: glong; screen_y: glong; client_x: glong; client_y: glong; ctrl_key: gboolean; alt_key: gboolean; shift_key: gboolean; meta_key: gboolean; button: gushort; related_target: PWebKitDOMEventTarget); cdecl; external;
procedure webkit_dom_navigator_get_storage_updates(ADOMNavigator: PWebKitDOMNavigator); cdecl; external;
procedure webkit_dom_node_iterator_detach(ADOMNodeIterator: PWebKitDOMNodeIterator); cdecl; external;
procedure webkit_dom_node_normalize(ADOMNode: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_node_set_node_value(ADOMNode: PWebKitDOMNode; value: Pgchar); cdecl; external;
procedure webkit_dom_node_set_prefix(ADOMNode: PWebKitDOMNode; value: Pgchar); cdecl; external;
procedure webkit_dom_node_set_text_content(ADOMNode: PWebKitDOMNode; value: Pgchar); cdecl; external;
procedure webkit_dom_processing_instruction_set_data(ADOMProcessingInstruction: PWebKitDOMProcessingInstruction; value: Pgchar); cdecl; external;
procedure webkit_dom_range_collapse(ADOMRange: PWebKitDOMRange; to_start: gboolean); cdecl; external;
procedure webkit_dom_range_delete_contents(ADOMRange: PWebKitDOMRange); cdecl; external;
procedure webkit_dom_range_detach(ADOMRange: PWebKitDOMRange); cdecl; external;
procedure webkit_dom_range_expand(ADOMRange: PWebKitDOMRange; unit_: Pgchar); cdecl; external;
procedure webkit_dom_range_insert_node(ADOMRange: PWebKitDOMRange; new_node: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_range_select_node(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_range_select_node_contents(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_range_set_end(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode; offset: glong); cdecl; external;
procedure webkit_dom_range_set_end_after(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_range_set_end_before(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_range_set_start(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode; offset: glong); cdecl; external;
procedure webkit_dom_range_set_start_after(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_range_set_start_before(ADOMRange: PWebKitDOMRange; ref_node: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_range_surround_contents(ADOMRange: PWebKitDOMRange; new_parent: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_storage_clear(ADOMStorage: PWebKitDOMStorage); cdecl; external;
procedure webkit_dom_storage_remove_item(ADOMStorage: PWebKitDOMStorage; key: Pgchar); cdecl; external;
procedure webkit_dom_storage_set_item(ADOMStorage: PWebKitDOMStorage; key: Pgchar; data: Pgchar); cdecl; external;
procedure webkit_dom_style_sheet_set_disabled(ADOMStyleSheet: PWebKitDOMStyleSheet; value: gboolean); cdecl; external;
procedure webkit_dom_tree_walker_set_current_node(ADOMTreeWalker: PWebKitDOMTreeWalker; value: PWebKitDOMNode); cdecl; external;
procedure webkit_dom_ui_event_init_ui_event(ADOMUIEvent: PWebKitDOMUIEvent; type_: Pgchar; can_bubble: gboolean; cancelable: gboolean; view: PWebKitDOMDOMWindow; detail: glong); cdecl; external;
procedure webkit_dom_webkit_animation_pause(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation); cdecl; external;
procedure webkit_dom_webkit_animation_play(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation); cdecl; external;
procedure webkit_dom_webkit_animation_set_elapsed_time(ADOMWebKitAnimation: PWebKitDOMWebKitAnimation; value: gdouble); cdecl; external;
procedure webkit_dom_webkit_point_set_x(ADOMWebKitPoint: PWebKitDOMWebKitPoint; value: gfloat); cdecl; external;
procedure webkit_dom_webkit_point_set_y(ADOMWebKitPoint: PWebKitDOMWebKitPoint; value: gfloat); cdecl; external;
procedure webkit_download_cancel(ADownload: PWebKitDownload); cdecl; external;
procedure webkit_download_set_destination_uri(ADownload: PWebKitDownload; destination_uri: Pgchar); cdecl; external;
procedure webkit_download_start(ADownload: PWebKitDownload); cdecl; external;
procedure webkit_geolocation_policy_allow(decision: PWebKitGeolocationPolicyDecision); cdecl; external;
procedure webkit_geolocation_policy_deny(decision: PWebKitGeolocationPolicyDecision); cdecl; external;
procedure webkit_icon_database_clear(AIconDatabase: PWebKitIconDatabase); cdecl; external;
procedure webkit_icon_database_set_path(AIconDatabase: PWebKitIconDatabase; path: Pgchar); cdecl; external;
procedure webkit_network_request_set_uri(ANetworkRequest: PWebKitNetworkRequest; uri: Pgchar); cdecl; external;
procedure webkit_network_response_set_uri(ANetworkResponse: PWebKitNetworkResponse; uri: Pgchar); cdecl; external;
procedure webkit_remove_all_web_databases; cdecl; external;
procedure webkit_security_origin_set_web_database_quota(ASecurityOrigin: PWebKitSecurityOrigin; quota: guint64); cdecl; external;
procedure webkit_set_cache_model(cache_model: TWebKitCacheModel); cdecl; external;
procedure webkit_set_default_web_database_quota(defaultQuota: guint64); cdecl; external;
procedure webkit_set_web_database_directory_path(path: Pgchar); cdecl; external;
procedure webkit_viewport_attributes_recompute(AViewportAttributes: PWebKitViewportAttributes); cdecl; external;
procedure webkit_web_back_forward_list_add_item(AWebBackForwardList: PWebKitWebBackForwardList; history_item: TWebKitWebHistoryItem); cdecl; external;
procedure webkit_web_back_forward_list_clear(AWebBackForwardList: PWebKitWebBackForwardList); cdecl; external;
procedure webkit_web_back_forward_list_go_back(AWebBackForwardList: PWebKitWebBackForwardList); cdecl; external;
procedure webkit_web_back_forward_list_go_forward(AWebBackForwardList: PWebKitWebBackForwardList); cdecl; external;
procedure webkit_web_back_forward_list_go_to_item(AWebBackForwardList: PWebKitWebBackForwardList; history_item: TWebKitWebHistoryItem); cdecl; external;
procedure webkit_web_back_forward_list_set_limit(AWebBackForwardList: PWebKitWebBackForwardList; limit: gint); cdecl; external;
procedure webkit_web_database_remove(AWebDatabase: PWebKitWebDatabase); cdecl; external;
procedure webkit_web_frame_load_alternate_string(AWebFrame: PWebKitWebFrame; content: Pgchar; base_url: Pgchar; unreachable_url: Pgchar); cdecl; external;
procedure webkit_web_frame_load_request(AWebFrame: PWebKitWebFrame; request: PWebKitNetworkRequest); cdecl; external;
procedure webkit_web_frame_load_string(AWebFrame: PWebKitWebFrame; content: Pgchar; mime_type: Pgchar; encoding: Pgchar; base_uri: Pgchar); cdecl; external;
procedure webkit_web_frame_load_uri(AWebFrame: PWebKitWebFrame; uri: Pgchar); cdecl; external;
procedure webkit_web_frame_print(AWebFrame: PWebKitWebFrame); cdecl; external;
procedure webkit_web_frame_reload(AWebFrame: PWebKitWebFrame); cdecl; external;
procedure webkit_web_frame_stop_loading(AWebFrame: PWebKitWebFrame); cdecl; external;
procedure webkit_web_history_item_set_alternate_title(AWebHistoryItem: PWebKitWebHistoryItem; title: Pgchar); cdecl; external;
procedure webkit_web_inspector_close(AWebInspector: PWebKitWebInspector); cdecl; external;
procedure webkit_web_inspector_inspect_coordinates(AWebInspector: PWebKitWebInspector; x: gdouble; y: gdouble); cdecl; external;
procedure webkit_web_inspector_inspect_node(AWebInspector: PWebKitWebInspector; node: PWebKitDOMNode); cdecl; external;
procedure webkit_web_inspector_show(AWebInspector: PWebKitWebInspector); cdecl; external;
procedure webkit_web_navigation_action_set_original_uri(AWebNavigationAction: PWebKitWebNavigationAction; originalUri: Pgchar); cdecl; external;
procedure webkit_web_navigation_action_set_reason(AWebNavigationAction: PWebKitWebNavigationAction; reason: TWebKitWebNavigationReason); cdecl; external;
procedure webkit_web_plugin_database_plugins_list_free(param0: PGSList); cdecl; external;
procedure webkit_web_plugin_database_refresh(AWebPluginDatabase: PWebKitWebPluginDatabase); cdecl; external;
procedure webkit_web_plugin_set_enabled(AWebPlugin: PWebKitWebPlugin; param0: gboolean); cdecl; external;
procedure webkit_web_policy_decision_download(AWebPolicyDecision: PWebKitWebPolicyDecision); cdecl; external;
procedure webkit_web_policy_decision_ignore(AWebPolicyDecision: PWebKitWebPolicyDecision); cdecl; external;
procedure webkit_web_policy_decision_use(AWebPolicyDecision: PWebKitWebPolicyDecision); cdecl; external;
procedure webkit_web_view_copy_clipboard(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_cut_clipboard(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_delete_selection(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_execute_script(AWebView: PWebKitWebView; script: Pgchar); cdecl; external;
procedure webkit_web_view_go_back(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_go_back_or_forward(AWebView: PWebKitWebView; steps: gint); cdecl; external;
procedure webkit_web_view_go_forward(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_load_html_string(AWebView: PWebKitWebView; content: Pgchar; base_uri: Pgchar); cdecl; external;
procedure webkit_web_view_load_request(AWebView: PWebKitWebView; request: PWebKitNetworkRequest); cdecl; external;
procedure webkit_web_view_load_string(AWebView: PWebKitWebView; content: Pgchar; mime_type: Pgchar; encoding: Pgchar; base_uri: Pgchar); cdecl; external;
procedure webkit_web_view_load_uri(AWebView: PWebKitWebView; uri: Pgchar); cdecl; external;
procedure webkit_web_view_move_cursor(AWebView: PWebKitWebView; step: TGtkMovementStep; count: gint); cdecl; external;
procedure webkit_web_view_open(AWebView: PWebKitWebView; uri: Pgchar); cdecl; external;
procedure webkit_web_view_paste_clipboard(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_redo(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_reload(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_reload_bypass_cache(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_select_all(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_set_custom_encoding(AWebView: PWebKitWebView; encoding: Pgchar); cdecl; external;
procedure webkit_web_view_set_editable(AWebView: PWebKitWebView; flag: gboolean); cdecl; external;
procedure webkit_web_view_set_full_content_zoom(AWebView: PWebKitWebView; full_content_zoom: gboolean); cdecl; external;
procedure webkit_web_view_set_highlight_text_matches(AWebView: PWebKitWebView; highlight: gboolean); cdecl; external;
procedure webkit_web_view_set_maintains_back_forward_list(AWebView: PWebKitWebView; flag: gboolean); cdecl; external;
procedure webkit_web_view_set_settings(AWebView: PWebKitWebView; settings: PWebKitWebSettings); cdecl; external;
procedure webkit_web_view_set_transparent(AWebView: PWebKitWebView; flag: gboolean); cdecl; external;
procedure webkit_web_view_set_view_mode(AWebView: PWebKitWebView; mode: TWebKitWebViewViewMode); cdecl; external;
procedure webkit_web_view_set_view_source_mode(AWebView: PWebKitWebView; view_source_mode: gboolean); cdecl; external;
procedure webkit_web_view_set_zoom_level(AWebView: PWebKitWebView; zoom_level: gfloat); cdecl; external;
procedure webkit_web_view_stop_loading(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_undo(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_unmark_text_matches(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_zoom_in(AWebView: PWebKitWebView); cdecl; external;
procedure webkit_web_view_zoom_out(AWebView: PWebKitWebView); cdecl; external;
implementation
function TWebKitDOMEventTarget.add_event_listener(eventName: Pgchar; handler: TGCallback; bubble: gboolean; userData: gpointer): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_event_target_add_event_listener(@self, eventName, handler, bubble, userData);
end;

procedure TWebKitDOMEventTarget.dispatch_event(event: PWebKitDOMEvent); cdecl;
begin
  WebKit3.webkit_dom_event_target_dispatch_event(@self, event);
end;

function TWebKitDOMEventTarget.remove_event_listener(eventName: Pgchar; handler: TGCallback; bubble: gboolean): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_event_target_remove_event_listener(@self, eventName, handler, bubble);
end;

function TWebKitDOMNode.append_child(new_child: PWebKitDOMNode): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_append_child(@self, new_child);
end;

function TWebKitDOMNode.clone_node(deep: gboolean): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_clone_node(@self, deep);
end;

function TWebKitDOMNode.compare_document_position(other: PWebKitDOMNode): gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_node_compare_document_position(@self, other);
end;

function TWebKitDOMNode.dispatch_event(event: PWebKitDOMEvent): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_dispatch_event(@self, event);
end;

function TWebKitDOMNode.get_attributes: PWebKitDOMNamedNodeMap; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_attributes(@self);
end;

function TWebKitDOMNode.get_base_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_base_uri(@self);
end;

function TWebKitDOMNode.get_child_nodes: PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_child_nodes(@self);
end;

function TWebKitDOMNode.get_first_child: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_first_child(@self);
end;

function TWebKitDOMNode.get_last_child: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_last_child(@self);
end;

function TWebKitDOMNode.get_local_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_local_name(@self);
end;

function TWebKitDOMNode.get_namespace_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_namespace_uri(@self);
end;

function TWebKitDOMNode.get_next_sibling: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_next_sibling(@self);
end;

function TWebKitDOMNode.get_node_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_node_name(@self);
end;

function TWebKitDOMNode.get_node_type: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_node_type(@self);
end;

function TWebKitDOMNode.get_node_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_node_value(@self);
end;

function TWebKitDOMNode.get_owner_document: PWebKitDOMDocument; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_owner_document(@self);
end;

function TWebKitDOMNode.get_parent_element: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_parent_element(@self);
end;

function TWebKitDOMNode.get_parent_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_parent_node(@self);
end;

function TWebKitDOMNode.get_prefix: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_prefix(@self);
end;

function TWebKitDOMNode.get_previous_sibling: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_previous_sibling(@self);
end;

function TWebKitDOMNode.get_text_content: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_get_text_content(@self);
end;

function TWebKitDOMNode.has_attributes: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_has_attributes(@self);
end;

function TWebKitDOMNode.has_child_nodes: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_has_child_nodes(@self);
end;

function TWebKitDOMNode.insert_before(new_child: PWebKitDOMNode; ref_child: PWebKitDOMNode): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_insert_before(@self, new_child, ref_child);
end;

function TWebKitDOMNode.is_default_namespace(namespace_uri: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_is_default_namespace(@self, namespace_uri);
end;

function TWebKitDOMNode.is_equal_node(other: PWebKitDOMNode): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_is_equal_node(@self, other);
end;

function TWebKitDOMNode.is_same_node(other: PWebKitDOMNode): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_is_same_node(@self, other);
end;

function TWebKitDOMNode.is_supported(feature: Pgchar; version: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_is_supported(@self, feature, version);
end;

function TWebKitDOMNode.lookup_namespace_uri(prefix: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_lookup_namespace_uri(@self, prefix);
end;

function TWebKitDOMNode.lookup_prefix(namespace_uri: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_node_lookup_prefix(@self, namespace_uri);
end;

procedure TWebKitDOMNode.normalize; cdecl;
begin
  WebKit3.webkit_dom_node_normalize(@self);
end;

function TWebKitDOMNode.remove_child(old_child: PWebKitDOMNode): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_remove_child(@self, old_child);
end;

function TWebKitDOMNode.replace_child(new_child: PWebKitDOMNode; old_child: PWebKitDOMNode): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_replace_child(@self, new_child, old_child);
end;

procedure TWebKitDOMNode.set_node_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_node_set_node_value(@self, value);
end;

procedure TWebKitDOMNode.set_prefix(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_node_set_prefix(@self, value);
end;

procedure TWebKitDOMNode.set_text_content(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_node_set_text_content(@self, value);
end;

procedure TWebKitDOMElement.blur; cdecl;
begin
  WebKit3.webkit_dom_element_blur(@self);
end;

function TWebKitDOMElement.contains(element: PWebKitDOMElement): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_element_contains(@self, element);
end;

procedure TWebKitDOMElement.focus; cdecl;
begin
  WebKit3.webkit_dom_element_focus(@self);
end;

function TWebKitDOMElement.get_attribute(name: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_attribute(@self, name);
end;

function TWebKitDOMElement.get_attribute_node(name: Pgchar): PWebKitDOMAttr; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_attribute_node(@self, name);
end;

function TWebKitDOMElement.get_attribute_node_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMAttr; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_attribute_node_ns(@self, namespace_uri, local_name);
end;

function TWebKitDOMElement.get_attribute_ns(namespace_uri: Pgchar; local_name: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_attribute_ns(@self, namespace_uri, local_name);
end;

function TWebKitDOMElement.get_child_element_count: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_child_element_count(@self);
end;

function TWebKitDOMElement.get_client_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_client_height(@self);
end;

function TWebKitDOMElement.get_client_left: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_client_left(@self);
end;

function TWebKitDOMElement.get_client_top: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_client_top(@self);
end;

function TWebKitDOMElement.get_client_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_client_width(@self);
end;

function TWebKitDOMElement.get_elements_by_class_name(name: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_elements_by_class_name(@self, name);
end;

function TWebKitDOMElement.get_elements_by_tag_name(name: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_elements_by_tag_name(@self, name);
end;

function TWebKitDOMElement.get_elements_by_tag_name_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_elements_by_tag_name_ns(@self, namespace_uri, local_name);
end;

function TWebKitDOMElement.get_first_element_child: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_first_element_child(@self);
end;

function TWebKitDOMElement.get_last_element_child: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_last_element_child(@self);
end;

function TWebKitDOMElement.get_next_element_sibling: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_next_element_sibling(@self);
end;

function TWebKitDOMElement.get_offset_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_offset_height(@self);
end;

function TWebKitDOMElement.get_offset_left: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_offset_left(@self);
end;

function TWebKitDOMElement.get_offset_parent: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_offset_parent(@self);
end;

function TWebKitDOMElement.get_offset_top: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_offset_top(@self);
end;

function TWebKitDOMElement.get_offset_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_offset_width(@self);
end;

function TWebKitDOMElement.get_previous_element_sibling: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_previous_element_sibling(@self);
end;

function TWebKitDOMElement.get_scroll_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_scroll_height(@self);
end;

function TWebKitDOMElement.get_scroll_left: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_scroll_left(@self);
end;

function TWebKitDOMElement.get_scroll_top: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_scroll_top(@self);
end;

function TWebKitDOMElement.get_scroll_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_scroll_width(@self);
end;

function TWebKitDOMElement.get_style: PWebKitDOMCSSStyleDeclaration; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_style(@self);
end;

function TWebKitDOMElement.get_tag_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_element_get_tag_name(@self);
end;

function TWebKitDOMElement.has_attribute(name: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_element_has_attribute(@self, name);
end;

function TWebKitDOMElement.has_attribute_ns(namespace_uri: Pgchar; local_name: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_element_has_attribute_ns(@self, namespace_uri, local_name);
end;

function TWebKitDOMElement.query_selector(selectors: Pgchar): PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_element_query_selector(@self, selectors);
end;

function TWebKitDOMElement.query_selector_all(selectors: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_element_query_selector_all(@self, selectors);
end;

procedure TWebKitDOMElement.remove_attribute(name: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_element_remove_attribute(@self, name);
end;

function TWebKitDOMElement.remove_attribute_node(old_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl;
begin
  Result := WebKit3.webkit_dom_element_remove_attribute_node(@self, old_attr);
end;

procedure TWebKitDOMElement.remove_attribute_ns(namespace_uri: Pgchar; local_name: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_element_remove_attribute_ns(@self, namespace_uri, local_name);
end;

procedure TWebKitDOMElement.scroll_by_lines(lines: glong); cdecl;
begin
  WebKit3.webkit_dom_element_scroll_by_lines(@self, lines);
end;

procedure TWebKitDOMElement.scroll_by_pages(pages: glong); cdecl;
begin
  WebKit3.webkit_dom_element_scroll_by_pages(@self, pages);
end;

procedure TWebKitDOMElement.scroll_into_view(align_with_top: gboolean); cdecl;
begin
  WebKit3.webkit_dom_element_scroll_into_view(@self, align_with_top);
end;

procedure TWebKitDOMElement.scroll_into_view_if_needed(center_if_needed: gboolean); cdecl;
begin
  WebKit3.webkit_dom_element_scroll_into_view_if_needed(@self, center_if_needed);
end;

procedure TWebKitDOMElement.set_attribute(name: Pgchar; value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_element_set_attribute(@self, name, value);
end;

function TWebKitDOMElement.set_attribute_node(new_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl;
begin
  Result := WebKit3.webkit_dom_element_set_attribute_node(@self, new_attr);
end;

function TWebKitDOMElement.set_attribute_node_ns(new_attr: PWebKitDOMAttr): PWebKitDOMAttr; cdecl;
begin
  Result := WebKit3.webkit_dom_element_set_attribute_node_ns(@self, new_attr);
end;

procedure TWebKitDOMElement.set_attribute_ns(namespace_uri: Pgchar; qualified_name: Pgchar; value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_element_set_attribute_ns(@self, namespace_uri, qualified_name, value);
end;

procedure TWebKitDOMElement.set_scroll_left(value: glong); cdecl;
begin
  WebKit3.webkit_dom_element_set_scroll_left(@self, value);
end;

procedure TWebKitDOMElement.set_scroll_top(value: glong); cdecl;
begin
  WebKit3.webkit_dom_element_set_scroll_top(@self, value);
end;

function TWebKitDOMElement.webkit_get_animations: PWebKitDOMWebKitAnimationList; cdecl;
begin
  Result := WebKit3.webkit_dom_element_webkit_get_animations(@self);
end;

function TWebKitDOMElement.webkit_matches_selector(selectors: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_element_webkit_matches_selector(@self, selectors);
end;

procedure TWebKitDOMElement.webkit_request_full_screen(flags: gushort); cdecl;
begin
  WebKit3.webkit_dom_element_webkit_request_full_screen(@self, flags);
end;

function TWebKitDOMAttr.get_is_id: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_attr_get_is_id(@self);
end;

function TWebKitDOMAttr.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_attr_get_name(@self);
end;

function TWebKitDOMAttr.get_owner_element: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_attr_get_owner_element(@self);
end;

function TWebKitDOMAttr.get_specified: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_attr_get_specified(@self);
end;

function TWebKitDOMAttr.get_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_attr_get_value(@self);
end;

procedure TWebKitDOMAttr.set_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_attr_set_value(@self, value);
end;

function TWebKitDOMBarInfo.get_visible: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_bar_info_get_visible(@self);
end;

function TWebKitDOMBlob.get_size: guint64; cdecl;
begin
  Result := WebKit3.webkit_dom_blob_get_size(@self);
end;

function TWebKitDOMBlob.slice(start: gint64; length: gint64; content_type: Pgchar): PWebKitDOMBlob; cdecl;
begin
  Result := WebKit3.webkit_dom_blob_slice(@self, start, length, content_type);
end;

procedure TWebKitDOMCharacterData.append_data(data: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_character_data_append_data(@self, data);
end;

procedure TWebKitDOMCharacterData.delete_data(offset: gulong; length: gulong); cdecl;
begin
  WebKit3.webkit_dom_character_data_delete_data(@self, offset, length);
end;

function TWebKitDOMCharacterData.get_data: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_character_data_get_data(@self);
end;

function TWebKitDOMCharacterData.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_character_data_get_length(@self);
end;

procedure TWebKitDOMCharacterData.insert_data(offset: gulong; data: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_character_data_insert_data(@self, offset, data);
end;

procedure TWebKitDOMCharacterData.replace_data(offset: gulong; length: gulong; data: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_character_data_replace_data(@self, offset, length, data);
end;

procedure TWebKitDOMCharacterData.set_data(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_character_data_set_data(@self, value);
end;

function TWebKitDOMCharacterData.substring_data(offset: gulong; length: gulong): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_character_data_substring_data(@self, offset, length);
end;

function TWebKitDOMText.get_whole_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_text_get_whole_text(@self);
end;

function TWebKitDOMText.replace_whole_text(content: Pgchar): PWebKitDOMText; cdecl;
begin
  Result := WebKit3.webkit_dom_text_replace_whole_text(@self, content);
end;

function TWebKitDOMText.split_text(offset: gulong): PWebKitDOMText; cdecl;
begin
  Result := WebKit3.webkit_dom_text_split_text(@self, offset);
end;

function TWebKitDOMCSSRule.get_css_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_css_rule_get_css_text(@self);
end;

function TWebKitDOMCSSRule.get_parent_rule: PWebKitDOMCSSRule; cdecl;
begin
  Result := WebKit3.webkit_dom_css_rule_get_parent_rule(@self);
end;

function TWebKitDOMCSSRule.get_parent_style_sheet: PWebKitDOMCSSStyleSheet; cdecl;
begin
  Result := WebKit3.webkit_dom_css_rule_get_parent_style_sheet(@self);
end;

procedure TWebKitDOMCSSRule.set_css_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_css_rule_set_css_text(@self, value);
end;

function TWebKitDOMStyleSheet.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_style_sheet_get_disabled(@self);
end;

function TWebKitDOMStyleSheet.get_href: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_style_sheet_get_href(@self);
end;

function TWebKitDOMStyleSheet.get_media: PWebKitDOMMediaList; cdecl;
begin
  Result := WebKit3.webkit_dom_style_sheet_get_media(@self);
end;

function TWebKitDOMStyleSheet.get_owner_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_style_sheet_get_owner_node(@self);
end;

function TWebKitDOMStyleSheet.get_parent_style_sheet: PWebKitDOMStyleSheet; cdecl;
begin
  Result := WebKit3.webkit_dom_style_sheet_get_parent_style_sheet(@self);
end;

function TWebKitDOMStyleSheet.get_title: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_style_sheet_get_title(@self);
end;

procedure TWebKitDOMStyleSheet.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_style_sheet_set_disabled(@self, value);
end;

function TWebKitDOMCSSStyleSheet.add_rule(selector: Pgchar; style: Pgchar; index: gulong): glong; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_sheet_add_rule(@self, selector, style, index);
end;

procedure TWebKitDOMCSSStyleSheet.delete_rule(index: gulong); cdecl;
begin
  WebKit3.webkit_dom_css_style_sheet_delete_rule(@self, index);
end;

function TWebKitDOMCSSStyleSheet.get_css_rules: PWebKitDOMCSSRuleList; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_sheet_get_css_rules(@self);
end;

function TWebKitDOMCSSStyleSheet.get_owner_rule: PWebKitDOMCSSRule; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_sheet_get_owner_rule(@self);
end;

function TWebKitDOMCSSStyleSheet.get_rules: PWebKitDOMCSSRuleList; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_sheet_get_rules(@self);
end;

function TWebKitDOMCSSStyleSheet.insert_rule(rule: Pgchar; index: gulong): gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_sheet_insert_rule(@self, rule, index);
end;

procedure TWebKitDOMCSSStyleSheet.remove_rule(index: gulong); cdecl;
begin
  WebKit3.webkit_dom_css_style_sheet_remove_rule(@self, index);
end;

function TWebKitDOMCSSRuleList.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_css_rule_list_get_length(@self);
end;

function TWebKitDOMCSSRuleList.item(index: gulong): PWebKitDOMCSSRule; cdecl;
begin
  Result := WebKit3.webkit_dom_css_rule_list_item(@self, index);
end;

function TWebKitDOMCSSValue.get_css_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_css_value_get_css_text(@self);
end;

function TWebKitDOMCSSValue.get_css_value_type: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_css_value_get_css_value_type(@self);
end;

procedure TWebKitDOMCSSValue.set_css_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_css_value_set_css_text(@self, value);
end;

function TWebKitDOMCSSStyleDeclaration.get_css_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_get_css_text(@self);
end;

function TWebKitDOMCSSStyleDeclaration.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_get_length(@self);
end;

function TWebKitDOMCSSStyleDeclaration.get_parent_rule: PWebKitDOMCSSRule; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_get_parent_rule(@self);
end;

function TWebKitDOMCSSStyleDeclaration.get_property_css_value(property_name: Pgchar): PWebKitDOMCSSValue; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_get_property_css_value(@self, property_name);
end;

function TWebKitDOMCSSStyleDeclaration.get_property_priority(property_name: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_get_property_priority(@self, property_name);
end;

function TWebKitDOMCSSStyleDeclaration.get_property_shorthand(property_name: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_get_property_shorthand(@self, property_name);
end;

function TWebKitDOMCSSStyleDeclaration.get_property_value(property_name: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_get_property_value(@self, property_name);
end;

function TWebKitDOMCSSStyleDeclaration.is_property_implicit(property_name: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_is_property_implicit(@self, property_name);
end;

function TWebKitDOMCSSStyleDeclaration.item(index: gulong): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_item(@self, index);
end;

function TWebKitDOMCSSStyleDeclaration.remove_property(property_name: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_css_style_declaration_remove_property(@self, property_name);
end;

procedure TWebKitDOMCSSStyleDeclaration.set_css_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_css_style_declaration_set_css_text(@self, value);
end;

procedure TWebKitDOMCSSStyleDeclaration.set_property(property_name: Pgchar; value: Pgchar; priority: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_css_style_declaration_set_property(@self, property_name, value, priority);
end;

function TWebKitDOMMemoryInfo.get_js_heap_size_limit: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_memory_info_get_js_heap_size_limit(@self);
end;

function TWebKitDOMMemoryInfo.get_total_js_heap_size: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_memory_info_get_total_js_heap_size(@self);
end;

function TWebKitDOMMemoryInfo.get_used_js_heap_size: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_memory_info_get_used_js_heap_size(@self);
end;

function TWebKitDOMConsole.get_memory: PWebKitDOMMemoryInfo; cdecl;
begin
  Result := WebKit3.webkit_dom_console_get_memory(@self);
end;

procedure TWebKitDOMConsole.group_end; cdecl;
begin
  WebKit3.webkit_dom_console_group_end(@self);
end;

procedure TWebKitDOMConsole.time(title: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_console_time(@self, title);
end;

function TWebKitDOMEvent.get_bubbles: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_bubbles(@self);
end;

function TWebKitDOMEvent.get_cancel_bubble: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_cancel_bubble(@self);
end;

function TWebKitDOMEvent.get_cancelable: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_cancelable(@self);
end;

function TWebKitDOMEvent.get_current_target: PWebKitDOMEventTarget; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_current_target(@self);
end;

function TWebKitDOMEvent.get_default_prevented: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_default_prevented(@self);
end;

function TWebKitDOMEvent.get_event_phase: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_event_phase(@self);
end;

function TWebKitDOMEvent.get_return_value: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_return_value(@self);
end;

function TWebKitDOMEvent.get_src_element: PWebKitDOMEventTarget; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_src_element(@self);
end;

function TWebKitDOMEvent.get_target: PWebKitDOMEventTarget; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_target(@self);
end;

function TWebKitDOMEvent.get_time_stamp: guint32; cdecl;
begin
  Result := WebKit3.webkit_dom_event_get_time_stamp(@self);
end;

procedure TWebKitDOMEvent.init_event(event_type_arg: Pgchar; can_bubble_arg: gboolean; cancelable_arg: gboolean); cdecl;
begin
  WebKit3.webkit_dom_event_init_event(@self, event_type_arg, can_bubble_arg, cancelable_arg);
end;

procedure TWebKitDOMEvent.prevent_default; cdecl;
begin
  WebKit3.webkit_dom_event_prevent_default(@self);
end;

procedure TWebKitDOMEvent.set_cancel_bubble(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_event_set_cancel_bubble(@self, value);
end;

procedure TWebKitDOMEvent.set_return_value(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_event_set_return_value(@self, value);
end;

procedure TWebKitDOMEvent.stop_immediate_propagation; cdecl;
begin
  WebKit3.webkit_dom_event_stop_immediate_propagation(@self);
end;

procedure TWebKitDOMEvent.stop_propagation; cdecl;
begin
  WebKit3.webkit_dom_event_stop_propagation(@self);
end;

function TWebKitDOMDOMApplicationCache.dispatch_event(evt: PWebKitDOMEvent): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_application_cache_dispatch_event(@self, evt);
end;

function TWebKitDOMDOMApplicationCache.get_status: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_application_cache_get_status(@self);
end;

procedure TWebKitDOMDOMApplicationCache.swap_cache; cdecl;
begin
  WebKit3.webkit_dom_dom_application_cache_swap_cache(@self);
end;

procedure TWebKitDOMDOMApplicationCache.update; cdecl;
begin
  WebKit3.webkit_dom_dom_application_cache_update(@self);
end;

function TWebKitDOMDocument.adopt_node(source: PWebKitDOMNode): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_document_adopt_node(@self, source);
end;

function TWebKitDOMDocument.caret_range_from_point(x: glong; y: glong): PWebKitDOMRange; cdecl;
begin
  Result := WebKit3.webkit_dom_document_caret_range_from_point(@self, x, y);
end;

function TWebKitDOMDocument.create_attribute(name: Pgchar): PWebKitDOMAttr; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_attribute(@self, name);
end;

function TWebKitDOMDocument.create_attribute_ns(namespace_uri: Pgchar; qualified_name: Pgchar): PWebKitDOMAttr; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_attribute_ns(@self, namespace_uri, qualified_name);
end;

function TWebKitDOMDocument.create_cdata_section(data: Pgchar): PWebKitDOMCDATASection; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_cdata_section(@self, data);
end;

function TWebKitDOMDocument.create_comment(data: Pgchar): PWebKitDOMComment; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_comment(@self, data);
end;

function TWebKitDOMDocument.create_css_style_declaration: PWebKitDOMCSSStyleDeclaration; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_css_style_declaration(@self);
end;

function TWebKitDOMDocument.create_document_fragment: PWebKitDOMDocumentFragment; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_document_fragment(@self);
end;

function TWebKitDOMDocument.create_element(tag_name: Pgchar): PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_element(@self, tag_name);
end;

function TWebKitDOMDocument.create_element_ns(namespace_uri: Pgchar; qualified_name: Pgchar): PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_element_ns(@self, namespace_uri, qualified_name);
end;

function TWebKitDOMDocument.create_entity_reference(name: Pgchar): PWebKitDOMEntityReference; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_entity_reference(@self, name);
end;

function TWebKitDOMDocument.create_event(event_type: Pgchar): PWebKitDOMEvent; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_event(@self, event_type);
end;

function TWebKitDOMDocument.create_expression(expression: Pgchar; resolver: PWebKitDOMXPathNSResolver): PWebKitDOMXPathExpression; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_expression(@self, expression, resolver);
end;

function TWebKitDOMDocument.create_node_iterator(root: PWebKitDOMNode; what_to_show: gulong; filter: PWebKitDOMNodeFilter; expand_entity_references: gboolean): PWebKitDOMNodeIterator; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_node_iterator(@self, root, what_to_show, filter, expand_entity_references);
end;

function TWebKitDOMDocument.create_ns_resolver(node_resolver: PWebKitDOMNode): PWebKitDOMXPathNSResolver; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_ns_resolver(@self, node_resolver);
end;

function TWebKitDOMDocument.create_processing_instruction(target: Pgchar; data: Pgchar): PWebKitDOMProcessingInstruction; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_processing_instruction(@self, target, data);
end;

function TWebKitDOMDocument.create_range: PWebKitDOMRange; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_range(@self);
end;

function TWebKitDOMDocument.create_text_node(data: Pgchar): PWebKitDOMText; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_text_node(@self, data);
end;

function TWebKitDOMDocument.create_tree_walker(root: PWebKitDOMNode; what_to_show: gulong; filter: PWebKitDOMNodeFilter; expand_entity_references: gboolean): PWebKitDOMTreeWalker; cdecl;
begin
  Result := WebKit3.webkit_dom_document_create_tree_walker(@self, root, what_to_show, filter, expand_entity_references);
end;

function TWebKitDOMDocument.element_from_point(x: glong; y: glong): PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_element_from_point(@self, x, y);
end;

function TWebKitDOMDocument.evaluate(expression: Pgchar; context_node: PWebKitDOMNode; resolver: PWebKitDOMXPathNSResolver; type_: gushort; in_result: PWebKitDOMXPathResult): PWebKitDOMXPathResult; cdecl;
begin
  Result := WebKit3.webkit_dom_document_evaluate(@self, expression, context_node, resolver, type_, in_result);
end;

function TWebKitDOMDocument.exec_command(command: Pgchar; user_interface: gboolean; value: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_document_exec_command(@self, command, user_interface, value);
end;

function TWebKitDOMDocument.get_anchors: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_anchors(@self);
end;

function TWebKitDOMDocument.get_applets: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_applets(@self);
end;

function TWebKitDOMDocument.get_body: PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_body(@self);
end;

function TWebKitDOMDocument.get_character_set: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_character_set(@self);
end;

function TWebKitDOMDocument.get_charset: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_charset(@self);
end;

function TWebKitDOMDocument.get_compat_mode: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_compat_mode(@self);
end;

function TWebKitDOMDocument.get_cookie: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_cookie(@self);
end;

function TWebKitDOMDocument.get_default_charset: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_default_charset(@self);
end;

function TWebKitDOMDocument.get_default_view: PWebKitDOMDOMWindow; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_default_view(@self);
end;

function TWebKitDOMDocument.get_doctype: PWebKitDOMDocumentType; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_doctype(@self);
end;

function TWebKitDOMDocument.get_document_element: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_document_element(@self);
end;

function TWebKitDOMDocument.get_document_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_document_uri(@self);
end;

function TWebKitDOMDocument.get_domain: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_domain(@self);
end;

function TWebKitDOMDocument.get_element_by_id(element_id: Pgchar): PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_element_by_id(@self, element_id);
end;

function TWebKitDOMDocument.get_elements_by_class_name(tagname: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_elements_by_class_name(@self, tagname);
end;

function TWebKitDOMDocument.get_elements_by_name(element_name: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_elements_by_name(@self, element_name);
end;

function TWebKitDOMDocument.get_elements_by_tag_name(tagname: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_elements_by_tag_name(@self, tagname);
end;

function TWebKitDOMDocument.get_elements_by_tag_name_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_elements_by_tag_name_ns(@self, namespace_uri, local_name);
end;

function TWebKitDOMDocument.get_forms: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_forms(@self);
end;

function TWebKitDOMDocument.get_head: PWebKitDOMHTMLHeadElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_head(@self);
end;

function TWebKitDOMDocument.get_images: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_images(@self);
end;

function TWebKitDOMDocument.get_implementation: PWebKitDOMDOMImplementation; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_implementation(@self);
end;

function TWebKitDOMDocument.get_input_encoding: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_input_encoding(@self);
end;

function TWebKitDOMDocument.get_last_modified: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_last_modified(@self);
end;

function TWebKitDOMDocument.get_links: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_links(@self);
end;

function TWebKitDOMDocument.get_override_style(element: PWebKitDOMElement; pseudo_element: Pgchar): PWebKitDOMCSSStyleDeclaration; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_override_style(@self, element, pseudo_element);
end;

function TWebKitDOMDocument.get_preferred_stylesheet_set: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_preferred_stylesheet_set(@self);
end;

function TWebKitDOMDocument.get_ready_state: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_ready_state(@self);
end;

function TWebKitDOMDocument.get_referrer: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_referrer(@self);
end;

function TWebKitDOMDocument.get_selected_stylesheet_set: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_selected_stylesheet_set(@self);
end;

function TWebKitDOMDocument.get_style_sheets: PWebKitDOMStyleSheetList; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_style_sheets(@self);
end;

function TWebKitDOMDocument.get_title: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_title(@self);
end;

function TWebKitDOMDocument.get_webkit_current_full_screen_element: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_webkit_current_full_screen_element(@self);
end;

function TWebKitDOMDocument.get_webkit_full_screen_keyboard_input_allowed: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_webkit_full_screen_keyboard_input_allowed(@self);
end;

function TWebKitDOMDocument.get_webkit_is_full_screen: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_webkit_is_full_screen(@self);
end;

function TWebKitDOMDocument.get_xml_encoding: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_xml_encoding(@self);
end;

function TWebKitDOMDocument.get_xml_standalone: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_xml_standalone(@self);
end;

function TWebKitDOMDocument.get_xml_version: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_get_xml_version(@self);
end;

function TWebKitDOMDocument.import_node(imported_node: PWebKitDOMNode; deep: gboolean): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_document_import_node(@self, imported_node, deep);
end;

function TWebKitDOMDocument.query_command_enabled(command: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_document_query_command_enabled(@self, command);
end;

function TWebKitDOMDocument.query_command_indeterm(command: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_document_query_command_indeterm(@self, command);
end;

function TWebKitDOMDocument.query_command_state(command: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_document_query_command_state(@self, command);
end;

function TWebKitDOMDocument.query_command_supported(command: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_document_query_command_supported(@self, command);
end;

function TWebKitDOMDocument.query_command_value(command: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_query_command_value(@self, command);
end;

function TWebKitDOMDocument.query_selector(selectors: Pgchar): PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_query_selector(@self, selectors);
end;

function TWebKitDOMDocument.query_selector_all(selectors: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_document_query_selector_all(@self, selectors);
end;

procedure TWebKitDOMDocument.set_body(value: PWebKitDOMHTMLElement); cdecl;
begin
  WebKit3.webkit_dom_document_set_body(@self, value);
end;

procedure TWebKitDOMDocument.set_charset(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_document_set_charset(@self, value);
end;

procedure TWebKitDOMDocument.set_cookie(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_document_set_cookie(@self, value);
end;

procedure TWebKitDOMDocument.set_document_uri(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_document_set_document_uri(@self, value);
end;

procedure TWebKitDOMDocument.set_selected_stylesheet_set(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_document_set_selected_stylesheet_set(@self, value);
end;

procedure TWebKitDOMDocument.set_title(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_document_set_title(@self, value);
end;

procedure TWebKitDOMDocument.set_xml_standalone(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_document_set_xml_standalone(@self, value);
end;

procedure TWebKitDOMDocument.set_xml_version(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_document_set_xml_version(@self, value);
end;

procedure TWebKitDOMDocument.webkit_cancel_full_screen; cdecl;
begin
  WebKit3.webkit_dom_document_webkit_cancel_full_screen(@self);
end;

function TWebKitDOMDocumentType.get_entities: PWebKitDOMNamedNodeMap; cdecl;
begin
  Result := WebKit3.webkit_dom_document_type_get_entities(@self);
end;

function TWebKitDOMDocumentType.get_internal_subset: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_type_get_internal_subset(@self);
end;

function TWebKitDOMDocumentType.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_type_get_name(@self);
end;

function TWebKitDOMDocumentType.get_notations: PWebKitDOMNamedNodeMap; cdecl;
begin
  Result := WebKit3.webkit_dom_document_type_get_notations(@self);
end;

function TWebKitDOMDocumentType.get_public_id: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_type_get_public_id(@self);
end;

function TWebKitDOMDocumentType.get_system_id: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_document_type_get_system_id(@self);
end;

procedure TWebKitDOMHTMLDocument.capture_events; cdecl;
begin
  WebKit3.webkit_dom_html_document_capture_events(@self);
end;

procedure TWebKitDOMHTMLDocument.clear; cdecl;
begin
  WebKit3.webkit_dom_html_document_clear(@self);
end;

procedure TWebKitDOMHTMLDocument.close; cdecl;
begin
  WebKit3.webkit_dom_html_document_close(@self);
end;

function TWebKitDOMHTMLDocument.get_active_element: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_active_element(@self);
end;

function TWebKitDOMHTMLDocument.get_alink_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_alink_color(@self);
end;

function TWebKitDOMHTMLDocument.get_bg_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_bg_color(@self);
end;

function TWebKitDOMHTMLDocument.get_compat_mode: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_compat_mode(@self);
end;

function TWebKitDOMHTMLDocument.get_design_mode: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_design_mode(@self);
end;

function TWebKitDOMHTMLDocument.get_dir: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_dir(@self);
end;

function TWebKitDOMHTMLDocument.get_embeds: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_embeds(@self);
end;

function TWebKitDOMHTMLDocument.get_fg_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_fg_color(@self);
end;

function TWebKitDOMHTMLDocument.get_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_height(@self);
end;

function TWebKitDOMHTMLDocument.get_link_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_link_color(@self);
end;

function TWebKitDOMHTMLDocument.get_plugins: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_plugins(@self);
end;

function TWebKitDOMHTMLDocument.get_scripts: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_scripts(@self);
end;

function TWebKitDOMHTMLDocument.get_vlink_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_vlink_color(@self);
end;

function TWebKitDOMHTMLDocument.get_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_get_width(@self);
end;

function TWebKitDOMHTMLDocument.has_focus: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_document_has_focus(@self);
end;

procedure TWebKitDOMHTMLDocument.release_events; cdecl;
begin
  WebKit3.webkit_dom_html_document_release_events(@self);
end;

procedure TWebKitDOMHTMLDocument.set_alink_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_document_set_alink_color(@self, value);
end;

procedure TWebKitDOMHTMLDocument.set_bg_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_document_set_bg_color(@self, value);
end;

procedure TWebKitDOMHTMLDocument.set_design_mode(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_document_set_design_mode(@self, value);
end;

procedure TWebKitDOMHTMLDocument.set_dir(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_document_set_dir(@self, value);
end;

procedure TWebKitDOMHTMLDocument.set_fg_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_document_set_fg_color(@self, value);
end;

procedure TWebKitDOMHTMLDocument.set_link_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_document_set_link_color(@self, value);
end;

procedure TWebKitDOMHTMLDocument.set_vlink_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_document_set_vlink_color(@self, value);
end;

function TWebKitDOMDOMImplementation.create_css_style_sheet(title: Pgchar; media: Pgchar): PWebKitDOMCSSStyleSheet; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_implementation_create_css_style_sheet(@self, title, media);
end;

function TWebKitDOMDOMImplementation.create_document(namespace_uri: Pgchar; qualified_name: Pgchar; doctype: PWebKitDOMDocumentType): PWebKitDOMDocument; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_implementation_create_document(@self, namespace_uri, qualified_name, doctype);
end;

function TWebKitDOMDOMImplementation.create_document_type(qualified_name: Pgchar; public_id: Pgchar; system_id: Pgchar): PWebKitDOMDocumentType; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_implementation_create_document_type(@self, qualified_name, public_id, system_id);
end;

function TWebKitDOMDOMImplementation.create_html_document(title: Pgchar): PWebKitDOMHTMLDocument; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_implementation_create_html_document(@self, title);
end;

function TWebKitDOMDOMImplementation.has_feature(feature: Pgchar; version: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_implementation_has_feature(@self, feature, version);
end;

function TWebKitDOMDOMPlugin.get_description: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_get_description(@self);
end;

function TWebKitDOMDOMPlugin.get_filename: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_get_filename(@self);
end;

function TWebKitDOMDOMPlugin.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_get_length(@self);
end;

function TWebKitDOMDOMPlugin.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_get_name(@self);
end;

function TWebKitDOMDOMPlugin.item(index: gulong): PWebKitDOMDOMMimeType; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_item(@self, index);
end;

function TWebKitDOMDOMPlugin.named_item(name: Pgchar): PWebKitDOMDOMMimeType; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_named_item(@self, name);
end;

function TWebKitDOMDOMMimeType.get_description: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_mime_type_get_description(@self);
end;

function TWebKitDOMDOMMimeType.get_enabled_plugin: PWebKitDOMDOMPlugin; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_mime_type_get_enabled_plugin(@self);
end;

function TWebKitDOMDOMMimeType.get_suffixes: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_mime_type_get_suffixes(@self);
end;

function TWebKitDOMDOMMimeTypeArray.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_mime_type_array_get_length(@self);
end;

function TWebKitDOMDOMMimeTypeArray.item(index: gulong): PWebKitDOMDOMMimeType; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_mime_type_array_item(@self, index);
end;

function TWebKitDOMDOMMimeTypeArray.named_item(name: Pgchar): PWebKitDOMDOMMimeType; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_mime_type_array_named_item(@self, name);
end;

function TWebKitDOMDOMPluginArray.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_array_get_length(@self);
end;

function TWebKitDOMDOMPluginArray.item(index: gulong): PWebKitDOMDOMPlugin; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_array_item(@self, index);
end;

function TWebKitDOMDOMPluginArray.named_item(name: Pgchar): PWebKitDOMDOMPlugin; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_plugin_array_named_item(@self, name);
end;

procedure TWebKitDOMDOMPluginArray.refresh(reload: gboolean); cdecl;
begin
  WebKit3.webkit_dom_dom_plugin_array_refresh(@self, reload);
end;

function TWebKitDOMRange.clone_contents: PWebKitDOMDocumentFragment; cdecl;
begin
  Result := WebKit3.webkit_dom_range_clone_contents(@self);
end;

function TWebKitDOMRange.clone_range: PWebKitDOMRange; cdecl;
begin
  Result := WebKit3.webkit_dom_range_clone_range(@self);
end;

procedure TWebKitDOMRange.collapse(to_start: gboolean); cdecl;
begin
  WebKit3.webkit_dom_range_collapse(@self, to_start);
end;

function TWebKitDOMRange.compare_boundary_points(how: gushort; source_range: PWebKitDOMRange): Tgshort; cdecl;
begin
  Result := WebKit3.webkit_dom_range_compare_boundary_points(@self, how, source_range);
end;

function TWebKitDOMRange.compare_node(ref_node: PWebKitDOMNode): Tgshort; cdecl;
begin
  Result := WebKit3.webkit_dom_range_compare_node(@self, ref_node);
end;

function TWebKitDOMRange.compare_point(ref_node: PWebKitDOMNode; offset: glong): Tgshort; cdecl;
begin
  Result := WebKit3.webkit_dom_range_compare_point(@self, ref_node, offset);
end;

function TWebKitDOMRange.create_contextual_fragment(html: Pgchar): PWebKitDOMDocumentFragment; cdecl;
begin
  Result := WebKit3.webkit_dom_range_create_contextual_fragment(@self, html);
end;

procedure TWebKitDOMRange.delete_contents; cdecl;
begin
  WebKit3.webkit_dom_range_delete_contents(@self);
end;

procedure TWebKitDOMRange.detach; cdecl;
begin
  WebKit3.webkit_dom_range_detach(@self);
end;

procedure TWebKitDOMRange.expand(unit_: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_range_expand(@self, unit_);
end;

function TWebKitDOMRange.extract_contents: PWebKitDOMDocumentFragment; cdecl;
begin
  Result := WebKit3.webkit_dom_range_extract_contents(@self);
end;

function TWebKitDOMRange.get_collapsed: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_range_get_collapsed(@self);
end;

function TWebKitDOMRange.get_common_ancestor_container: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_range_get_common_ancestor_container(@self);
end;

function TWebKitDOMRange.get_end_container: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_range_get_end_container(@self);
end;

function TWebKitDOMRange.get_end_offset: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_range_get_end_offset(@self);
end;

function TWebKitDOMRange.get_start_container: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_range_get_start_container(@self);
end;

function TWebKitDOMRange.get_start_offset: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_range_get_start_offset(@self);
end;

function TWebKitDOMRange.get_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_range_get_text(@self);
end;

procedure TWebKitDOMRange.insert_node(new_node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_range_insert_node(@self, new_node);
end;

function TWebKitDOMRange.intersects_node(ref_node: PWebKitDOMNode): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_range_intersects_node(@self, ref_node);
end;

function TWebKitDOMRange.is_point_in_range(ref_node: PWebKitDOMNode; offset: glong): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_range_is_point_in_range(@self, ref_node, offset);
end;

procedure TWebKitDOMRange.select_node(ref_node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_range_select_node(@self, ref_node);
end;

procedure TWebKitDOMRange.select_node_contents(ref_node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_range_select_node_contents(@self, ref_node);
end;

procedure TWebKitDOMRange.set_end(ref_node: PWebKitDOMNode; offset: glong); cdecl;
begin
  WebKit3.webkit_dom_range_set_end(@self, ref_node, offset);
end;

procedure TWebKitDOMRange.set_end_after(ref_node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_range_set_end_after(@self, ref_node);
end;

procedure TWebKitDOMRange.set_end_before(ref_node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_range_set_end_before(@self, ref_node);
end;

procedure TWebKitDOMRange.set_start(ref_node: PWebKitDOMNode; offset: glong); cdecl;
begin
  WebKit3.webkit_dom_range_set_start(@self, ref_node, offset);
end;

procedure TWebKitDOMRange.set_start_after(ref_node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_range_set_start_after(@self, ref_node);
end;

procedure TWebKitDOMRange.set_start_before(ref_node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_range_set_start_before(@self, ref_node);
end;

procedure TWebKitDOMRange.surround_contents(new_parent: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_range_surround_contents(@self, new_parent);
end;

function TWebKitDOMRange.to_string: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_range_to_string(@self);
end;

procedure TWebKitDOMDOMSelection.add_range(range: PWebKitDOMRange); cdecl;
begin
  WebKit3.webkit_dom_dom_selection_add_range(@self, range);
end;

procedure TWebKitDOMDOMSelection.collapse(node: PWebKitDOMNode; index: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_selection_collapse(@self, node, index);
end;

procedure TWebKitDOMDOMSelection.collapse_to_end; cdecl;
begin
  WebKit3.webkit_dom_dom_selection_collapse_to_end(@self);
end;

procedure TWebKitDOMDOMSelection.collapse_to_start; cdecl;
begin
  WebKit3.webkit_dom_dom_selection_collapse_to_start(@self);
end;

function TWebKitDOMDOMSelection.contains_node(node: PWebKitDOMNode; allow_partial: gboolean): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_contains_node(@self, node, allow_partial);
end;

procedure TWebKitDOMDOMSelection.delete_from_document; cdecl;
begin
  WebKit3.webkit_dom_dom_selection_delete_from_document(@self);
end;

procedure TWebKitDOMDOMSelection.empty; cdecl;
begin
  WebKit3.webkit_dom_dom_selection_empty(@self);
end;

procedure TWebKitDOMDOMSelection.extend(node: PWebKitDOMNode; offset: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_selection_extend(@self, node, offset);
end;

function TWebKitDOMDOMSelection.get_anchor_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_anchor_node(@self);
end;

function TWebKitDOMDOMSelection.get_anchor_offset: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_anchor_offset(@self);
end;

function TWebKitDOMDOMSelection.get_base_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_base_node(@self);
end;

function TWebKitDOMDOMSelection.get_base_offset: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_base_offset(@self);
end;

function TWebKitDOMDOMSelection.get_extent_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_extent_node(@self);
end;

function TWebKitDOMDOMSelection.get_extent_offset: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_extent_offset(@self);
end;

function TWebKitDOMDOMSelection.get_focus_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_focus_node(@self);
end;

function TWebKitDOMDOMSelection.get_focus_offset: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_focus_offset(@self);
end;

function TWebKitDOMDOMSelection.get_is_collapsed: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_is_collapsed(@self);
end;

function TWebKitDOMDOMSelection.get_range_at(index: glong): PWebKitDOMRange; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_range_at(@self, index);
end;

function TWebKitDOMDOMSelection.get_range_count: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_selection_get_range_count(@self);
end;

procedure TWebKitDOMDOMSelection.modify(alter: Pgchar; direction: Pgchar; granularity: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_dom_selection_modify(@self, alter, direction, granularity);
end;

procedure TWebKitDOMDOMSelection.remove_all_ranges; cdecl;
begin
  WebKit3.webkit_dom_dom_selection_remove_all_ranges(@self);
end;

procedure TWebKitDOMDOMSelection.select_all_children(node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_dom_selection_select_all_children(@self, node);
end;

procedure TWebKitDOMDOMSelection.set_base_and_extent(base_node: PWebKitDOMNode; base_offset: glong; extent_node: PWebKitDOMNode; extent_offset: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_selection_set_base_and_extent(@self, base_node, base_offset, extent_node, extent_offset);
end;

procedure TWebKitDOMDOMSelection.set_position(node: PWebKitDOMNode; offset: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_selection_set_position(@self, node, offset);
end;

procedure TWebKitDOMDOMTokenList.add(token: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_dom_token_list_add(@self, token);
end;

function TWebKitDOMDOMTokenList.contains(token: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_token_list_contains(@self, token);
end;

function TWebKitDOMDOMTokenList.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_token_list_get_length(@self);
end;

function TWebKitDOMDOMTokenList.item(index: gulong): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_token_list_item(@self, index);
end;

procedure TWebKitDOMDOMTokenList.remove(token: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_dom_token_list_remove(@self, token);
end;

function TWebKitDOMDOMTokenList.toggle(token: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_token_list_toggle(@self, token);
end;

function TWebKitDOMDOMSettableTokenList.get_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_settable_token_list_get_value(@self);
end;

procedure TWebKitDOMDOMSettableTokenList.set_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_dom_settable_token_list_set_value(@self, value);
end;

function TWebKitDOMDOMStringList.contains(string_: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_string_list_contains(@self, string_);
end;

function TWebKitDOMDOMStringList.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_string_list_get_length(@self);
end;

function TWebKitDOMDOMStringList.item(index: gulong): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_string_list_item(@self, index);
end;

procedure TWebKitDOMHistory.back; cdecl;
begin
  WebKit3.webkit_dom_history_back(@self);
end;

procedure TWebKitDOMHistory.forward; cdecl;
begin
  WebKit3.webkit_dom_history_forward(@self);
end;

function TWebKitDOMHistory.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_history_get_length(@self);
end;

procedure TWebKitDOMHistory.go(distance: glong); cdecl;
begin
  WebKit3.webkit_dom_history_go(@self, distance);
end;

procedure TWebKitDOMStorage.clear; cdecl;
begin
  WebKit3.webkit_dom_storage_clear(@self);
end;

function TWebKitDOMStorage.get_item(key: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_storage_get_item(@self, key);
end;

function TWebKitDOMStorage.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_storage_get_length(@self);
end;

function TWebKitDOMStorage.key(index: gulong): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_storage_key(@self, index);
end;

procedure TWebKitDOMStorage.remove_item(key: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_storage_remove_item(@self, key);
end;

procedure TWebKitDOMStorage.set_item(key: Pgchar; data: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_storage_set_item(@self, key, data);
end;

function TWebKitDOMStyleMedia.match_medium(mediaquery: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_style_media_match_medium(@self, mediaquery);
end;

procedure TWebKitDOMDOMWindow.alert(message: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_dom_window_alert(@self, message);
end;

function TWebKitDOMDOMWindow.atob(string_: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_atob(@self, string_);
end;

procedure TWebKitDOMDOMWindow.blur; cdecl;
begin
  WebKit3.webkit_dom_dom_window_blur(@self);
end;

function TWebKitDOMDOMWindow.btoa(string_: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_btoa(@self, string_);
end;

procedure TWebKitDOMDOMWindow.capture_events; cdecl;
begin
  WebKit3.webkit_dom_dom_window_capture_events(@self);
end;

procedure TWebKitDOMDOMWindow.clear_interval(handle: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_window_clear_interval(@self, handle);
end;

procedure TWebKitDOMDOMWindow.clear_timeout(handle: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_window_clear_timeout(@self, handle);
end;

procedure TWebKitDOMDOMWindow.close; cdecl;
begin
  WebKit3.webkit_dom_dom_window_close(@self);
end;

function TWebKitDOMDOMWindow.confirm(message: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_confirm(@self, message);
end;

function TWebKitDOMDOMWindow.dispatch_event(evt: PWebKitDOMEvent): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_dispatch_event(@self, evt);
end;

function TWebKitDOMDOMWindow.find(string_: Pgchar; case_sensitive: gboolean; backwards: gboolean; wrap: gboolean; whole_word: gboolean; search_in_frames: gboolean; show_dialog: gboolean): gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_find(@self, string_, case_sensitive, backwards, wrap, whole_word, search_in_frames, show_dialog);
end;

procedure TWebKitDOMDOMWindow.focus; cdecl;
begin
  WebKit3.webkit_dom_dom_window_focus(@self);
end;

function TWebKitDOMDOMWindow.get_application_cache: PWebKitDOMDOMApplicationCache; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_application_cache(@self);
end;

function TWebKitDOMDOMWindow.get_closed: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_closed(@self);
end;

function TWebKitDOMDOMWindow.get_computed_style(element: PWebKitDOMElement; pseudo_element: Pgchar): PWebKitDOMCSSStyleDeclaration; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_computed_style(@self, element, pseudo_element);
end;

function TWebKitDOMDOMWindow.get_default_status: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_default_status(@self);
end;

function TWebKitDOMDOMWindow.get_document: PWebKitDOMDocument; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_document(@self);
end;

function TWebKitDOMDOMWindow.get_frame_element: PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_frame_element(@self);
end;

function TWebKitDOMDOMWindow.get_history: PWebKitDOMHistory; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_history(@self);
end;

function TWebKitDOMDOMWindow.get_local_storage: PWebKitDOMStorage; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_local_storage(@self);
end;

function TWebKitDOMDOMWindow.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_name(@self);
end;

function TWebKitDOMDOMWindow.get_page_x_offset: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_page_x_offset(@self);
end;

function TWebKitDOMDOMWindow.get_page_y_offset: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_page_y_offset(@self);
end;

function TWebKitDOMDOMWindow.get_selection: PWebKitDOMDOMSelection; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_selection(@self);
end;

function TWebKitDOMDOMWindow.get_session_storage: PWebKitDOMStorage; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_session_storage(@self);
end;

function TWebKitDOMDOMWindow.get_status: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_status(@self);
end;

function TWebKitDOMDOMWindow.get_style_media: PWebKitDOMStyleMedia; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_style_media(@self);
end;

function TWebKitDOMDOMWindow.get_window: PWebKitDOMDOMWindow; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_get_window(@self);
end;

function TWebKitDOMDOMWindow.match_media(query: Pgchar): PWebKitDOMMediaQueryList; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_match_media(@self, query);
end;

procedure TWebKitDOMDOMWindow.move_by(x: gfloat; y: gfloat); cdecl;
begin
  WebKit3.webkit_dom_dom_window_move_by(@self, x, y);
end;

procedure TWebKitDOMDOMWindow.move_to(x: gfloat; y: gfloat); cdecl;
begin
  WebKit3.webkit_dom_dom_window_move_to(@self, x, y);
end;

procedure TWebKitDOMDOMWindow.print; cdecl;
begin
  WebKit3.webkit_dom_dom_window_print(@self);
end;

function TWebKitDOMDOMWindow.prompt(message: Pgchar; default_value: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_prompt(@self, message, default_value);
end;

procedure TWebKitDOMDOMWindow.release_events; cdecl;
begin
  WebKit3.webkit_dom_dom_window_release_events(@self);
end;

procedure TWebKitDOMDOMWindow.resize_by(x: gfloat; y: gfloat); cdecl;
begin
  WebKit3.webkit_dom_dom_window_resize_by(@self, x, y);
end;

procedure TWebKitDOMDOMWindow.resize_to(width: gfloat; height: gfloat); cdecl;
begin
  WebKit3.webkit_dom_dom_window_resize_to(@self, width, height);
end;

procedure TWebKitDOMDOMWindow.scroll(x: glong; y: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_window_scroll(@self, x, y);
end;

procedure TWebKitDOMDOMWindow.scroll_by(x: glong; y: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_window_scroll_by(@self, x, y);
end;

procedure TWebKitDOMDOMWindow.scroll_to(x: glong; y: glong); cdecl;
begin
  WebKit3.webkit_dom_dom_window_scroll_to(@self, x, y);
end;

procedure TWebKitDOMDOMWindow.set_default_status(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_dom_window_set_default_status(@self, value);
end;

procedure TWebKitDOMDOMWindow.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_dom_window_set_name(@self, value);
end;

procedure TWebKitDOMDOMWindow.set_status(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_dom_window_set_status(@self, value);
end;

procedure TWebKitDOMDOMWindow.stop; cdecl;
begin
  WebKit3.webkit_dom_dom_window_stop(@self);
end;

function TWebKitDOMDOMWindow.webkit_convert_point_from_node_to_page(node: PWebKitDOMNode; p: PWebKitDOMWebKitPoint): PWebKitDOMWebKitPoint; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_webkit_convert_point_from_node_to_page(@self, node, p);
end;

function TWebKitDOMDOMWindow.webkit_convert_point_from_page_to_node(node: PWebKitDOMNode; p: PWebKitDOMWebKitPoint): PWebKitDOMWebKitPoint; cdecl;
begin
  Result := WebKit3.webkit_dom_dom_window_webkit_convert_point_from_page_to_node(@self, node, p);
end;

function TWebKitDOMMediaQueryList.get_matches: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_media_query_list_get_matches(@self);
end;

function TWebKitDOMMediaQueryList.get_media: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_media_query_list_get_media(@self);
end;

function TWebKitDOMWebKitPoint.get_x: gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_point_get_x(@self);
end;

function TWebKitDOMWebKitPoint.get_y: gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_point_get_y(@self);
end;

procedure TWebKitDOMWebKitPoint.set_x(value: gfloat); cdecl;
begin
  WebKit3.webkit_dom_webkit_point_set_x(@self, value);
end;

procedure TWebKitDOMWebKitPoint.set_y(value: gfloat); cdecl;
begin
  WebKit3.webkit_dom_webkit_point_set_y(@self, value);
end;

function TWebKitDOMDatabase.get_version: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_database_get_version(@self);
end;

function TWebKitDOMDocumentFragment.query_selector(selectors: Pgchar): PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_document_fragment_query_selector(@self, selectors);
end;

function TWebKitDOMDocumentFragment.query_selector_all(selectors: Pgchar): PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_document_fragment_query_selector_all(@self, selectors);
end;

function TWebKitDOMXPathExpression.evaluate(context_node: PWebKitDOMNode; type_: gushort; in_result: PWebKitDOMXPathResult): PWebKitDOMXPathResult; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_expression_evaluate(@self, context_node, type_, in_result);
end;

function TWebKitDOMXPathNSResolver.lookup_namespace_uri(prefix: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_ns_resolver_lookup_namespace_uri(@self, prefix);
end;

procedure TWebKitDOMNodeIterator.detach; cdecl;
begin
  WebKit3.webkit_dom_node_iterator_detach(@self);
end;

function TWebKitDOMNodeIterator.get_expand_entity_references: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_iterator_get_expand_entity_references(@self);
end;

function TWebKitDOMNodeIterator.get_filter: PWebKitDOMNodeFilter; cdecl;
begin
  Result := WebKit3.webkit_dom_node_iterator_get_filter(@self);
end;

function TWebKitDOMNodeIterator.get_pointer_before_reference_node: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_node_iterator_get_pointer_before_reference_node(@self);
end;

function TWebKitDOMNodeIterator.get_reference_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_iterator_get_reference_node(@self);
end;

function TWebKitDOMNodeIterator.get_root: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_iterator_get_root(@self);
end;

function TWebKitDOMNodeIterator.get_what_to_show: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_node_iterator_get_what_to_show(@self);
end;

function TWebKitDOMNodeIterator.next_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_iterator_next_node(@self);
end;

function TWebKitDOMNodeIterator.previous_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_iterator_previous_node(@self);
end;

function TWebKitDOMNodeFilter.accept_node(n: PWebKitDOMNode): Tgshort; cdecl;
begin
  Result := WebKit3.webkit_dom_node_filter_accept_node(@self, n);
end;

function TWebKitDOMProcessingInstruction.get_data: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_processing_instruction_get_data(@self);
end;

function TWebKitDOMProcessingInstruction.get_sheet: PWebKitDOMStyleSheet; cdecl;
begin
  Result := WebKit3.webkit_dom_processing_instruction_get_sheet(@self);
end;

function TWebKitDOMProcessingInstruction.get_target: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_processing_instruction_get_target(@self);
end;

procedure TWebKitDOMProcessingInstruction.set_data(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_processing_instruction_set_data(@self, value);
end;

function TWebKitDOMTreeWalker.first_child: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_first_child(@self);
end;

function TWebKitDOMTreeWalker.get_current_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_get_current_node(@self);
end;

function TWebKitDOMTreeWalker.get_expand_entity_references: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_get_expand_entity_references(@self);
end;

function TWebKitDOMTreeWalker.get_filter: PWebKitDOMNodeFilter; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_get_filter(@self);
end;

function TWebKitDOMTreeWalker.get_root: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_get_root(@self);
end;

function TWebKitDOMTreeWalker.get_what_to_show: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_get_what_to_show(@self);
end;

function TWebKitDOMTreeWalker.last_child: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_last_child(@self);
end;

function TWebKitDOMTreeWalker.next_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_next_node(@self);
end;

function TWebKitDOMTreeWalker.next_sibling: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_next_sibling(@self);
end;

function TWebKitDOMTreeWalker.parent_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_parent_node(@self);
end;

function TWebKitDOMTreeWalker.previous_node: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_previous_node(@self);
end;

function TWebKitDOMTreeWalker.previous_sibling: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_tree_walker_previous_sibling(@self);
end;

procedure TWebKitDOMTreeWalker.set_current_node(value: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_dom_tree_walker_set_current_node(@self, value);
end;

function TWebKitDOMXPathResult.get_boolean_value: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_get_boolean_value(@self);
end;

function TWebKitDOMXPathResult.get_invalid_iterator_state: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_get_invalid_iterator_state(@self);
end;

function TWebKitDOMXPathResult.get_number_value: gdouble; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_get_number_value(@self);
end;

function TWebKitDOMXPathResult.get_result_type: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_get_result_type(@self);
end;

function TWebKitDOMXPathResult.get_single_node_value: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_get_single_node_value(@self);
end;

function TWebKitDOMXPathResult.get_snapshot_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_get_snapshot_length(@self);
end;

function TWebKitDOMXPathResult.get_string_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_get_string_value(@self);
end;

function TWebKitDOMXPathResult.iterate_next: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_iterate_next(@self);
end;

function TWebKitDOMXPathResult.snapshot_item(index: gulong): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_xpath_result_snapshot_item(@self, index);
end;

function TWebKitDOMHTMLCollection.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_collection_get_length(@self);
end;

function TWebKitDOMHTMLCollection.item(index: gulong): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_html_collection_item(@self, index);
end;

function TWebKitDOMHTMLCollection.named_item(name: Pgchar): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_html_collection_named_item(@self, name);
end;

function TWebKitDOMHTMLElement.get_children: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_children(@self);
end;

function TWebKitDOMHTMLElement.get_class_list: PWebKitDOMDOMTokenList; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_class_list(@self);
end;

function TWebKitDOMHTMLElement.get_class_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_class_name(@self);
end;

function TWebKitDOMHTMLElement.get_content_editable: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_content_editable(@self);
end;

function TWebKitDOMHTMLElement.get_dir: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_dir(@self);
end;

function TWebKitDOMHTMLElement.get_draggable: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_draggable(@self);
end;

function TWebKitDOMHTMLElement.get_hidden: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_hidden(@self);
end;

function TWebKitDOMHTMLElement.get_id: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_id(@self);
end;

function TWebKitDOMHTMLElement.get_inner_html: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_inner_html(@self);
end;

function TWebKitDOMHTMLElement.get_inner_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_inner_text(@self);
end;

function TWebKitDOMHTMLElement.get_is_content_editable: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_is_content_editable(@self);
end;

function TWebKitDOMHTMLElement.get_lang: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_lang(@self);
end;

function TWebKitDOMHTMLElement.get_outer_html: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_outer_html(@self);
end;

function TWebKitDOMHTMLElement.get_outer_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_outer_text(@self);
end;

function TWebKitDOMHTMLElement.get_spellcheck: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_spellcheck(@self);
end;

function TWebKitDOMHTMLElement.get_tab_index: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_tab_index(@self);
end;

function TWebKitDOMHTMLElement.get_title: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_get_title(@self);
end;

function TWebKitDOMHTMLElement.insert_adjacent_element(where: Pgchar; element: PWebKitDOMElement): PWebKitDOMElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_element_insert_adjacent_element(@self, where, element);
end;

procedure TWebKitDOMHTMLElement.insert_adjacent_html(where: Pgchar; html: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_insert_adjacent_html(@self, where, html);
end;

procedure TWebKitDOMHTMLElement.insert_adjacent_text(where: Pgchar; text: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_insert_adjacent_text(@self, where, text);
end;

procedure TWebKitDOMHTMLElement.set_class_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_class_name(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_content_editable(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_content_editable(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_dir(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_dir(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_draggable(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_draggable(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_hidden(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_hidden(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_id(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_id(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_inner_html(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_inner_html(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_inner_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_inner_text(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_lang(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_lang(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_outer_html(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_outer_html(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_outer_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_outer_text(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_spellcheck(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_spellcheck(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_tab_index(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_tab_index(@self, value);
end;

procedure TWebKitDOMHTMLElement.set_title(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_element_set_title(@self, value);
end;

function TWebKitDOMNodeList.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_node_list_get_length(@self);
end;

function TWebKitDOMNodeList.item(index: gulong): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_node_list_item(@self, index);
end;

function TWebKitDOMHTMLHeadElement.get_profile: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_head_element_get_profile(@self);
end;

procedure TWebKitDOMHTMLHeadElement.set_profile(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_head_element_set_profile(@self, value);
end;

function TWebKitDOMStyleSheetList.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_style_sheet_list_get_length(@self);
end;

function TWebKitDOMStyleSheetList.item(index: gulong): PWebKitDOMStyleSheet; cdecl;
begin
  Result := WebKit3.webkit_dom_style_sheet_list_item(@self, index);
end;

function TWebKitDOMNamedNodeMap.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_named_node_map_get_length(@self);
end;

function TWebKitDOMNamedNodeMap.get_named_item(name: Pgchar): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_named_node_map_get_named_item(@self, name);
end;

function TWebKitDOMNamedNodeMap.get_named_item_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_named_node_map_get_named_item_ns(@self, namespace_uri, local_name);
end;

function TWebKitDOMNamedNodeMap.item(index: gulong): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_named_node_map_item(@self, index);
end;

function TWebKitDOMNamedNodeMap.remove_named_item(name: Pgchar): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_named_node_map_remove_named_item(@self, name);
end;

function TWebKitDOMNamedNodeMap.remove_named_item_ns(namespace_uri: Pgchar; local_name: Pgchar): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_named_node_map_remove_named_item_ns(@self, namespace_uri, local_name);
end;

function TWebKitDOMNamedNodeMap.set_named_item(node: PWebKitDOMNode): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_named_node_map_set_named_item(@self, node);
end;

function TWebKitDOMNamedNodeMap.set_named_item_ns(node: PWebKitDOMNode): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_named_node_map_set_named_item_ns(@self, node);
end;

function TWebKitDOMWebKitAnimationList.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_list_get_length(@self);
end;

function TWebKitDOMWebKitAnimationList.item(index: gulong): PWebKitDOMWebKitAnimation; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_list_item(@self, index);
end;

function TWebKitDOMFile.get_file_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_file_get_file_name(@self);
end;

function TWebKitDOMFile.get_file_size: guint64; cdecl;
begin
  Result := WebKit3.webkit_dom_file_get_file_size(@self);
end;

function TWebKitDOMFile.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_file_get_name(@self);
end;

function TWebKitDOMFileList.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_file_list_get_length(@self);
end;

function TWebKitDOMFileList.item(index: gulong): PWebKitDOMFile; cdecl;
begin
  Result := WebKit3.webkit_dom_file_list_item(@self, index);
end;

function TWebKitDOMHTMLAnchorElement.get_access_key: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_access_key(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_charset: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_charset(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_coords: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_coords(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_hash: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_hash(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_host: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_host(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_hostname: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_hostname(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_href: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_href(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_hreflang: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_hreflang(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_name(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_origin: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_origin(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_parameter(name: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_parameter(@self, name);
end;

function TWebKitDOMHTMLAnchorElement.get_pathname: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_pathname(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_port: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_port(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_protocol: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_protocol(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_rel: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_rel(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_rev: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_rev(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_search: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_search(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_shape: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_shape(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_target: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_target(@self);
end;

function TWebKitDOMHTMLAnchorElement.get_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_anchor_element_get_text(@self);
end;

procedure TWebKitDOMHTMLAnchorElement.set_access_key(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_access_key(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_charset(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_charset(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_coords(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_coords(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_hash(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_hash(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_host(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_host(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_hostname(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_hostname(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_href(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_href(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_hreflang(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_hreflang(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_pathname(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_pathname(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_port(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_port(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_protocol(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_protocol(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_rel(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_rel(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_rev(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_rev(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_search(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_search(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_shape(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_shape(@self, value);
end;

procedure TWebKitDOMHTMLAnchorElement.set_target(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_anchor_element_set_target(@self, value);
end;

function TWebKitDOMHTMLAppletElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_align(@self);
end;

function TWebKitDOMHTMLAppletElement.get_alt: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_alt(@self);
end;

function TWebKitDOMHTMLAppletElement.get_archive: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_archive(@self);
end;

function TWebKitDOMHTMLAppletElement.get_code: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_code(@self);
end;

function TWebKitDOMHTMLAppletElement.get_code_base: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_code_base(@self);
end;

function TWebKitDOMHTMLAppletElement.get_height: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_height(@self);
end;

function TWebKitDOMHTMLAppletElement.get_hspace: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_hspace(@self);
end;

function TWebKitDOMHTMLAppletElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_name(@self);
end;

function TWebKitDOMHTMLAppletElement.get_object: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_object(@self);
end;

function TWebKitDOMHTMLAppletElement.get_vspace: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_vspace(@self);
end;

function TWebKitDOMHTMLAppletElement.get_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_applet_element_get_width(@self);
end;

procedure TWebKitDOMHTMLAppletElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_alt(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_alt(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_archive(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_archive(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_code(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_code(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_code_base(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_code_base(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_height(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_hspace(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_hspace(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_object(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_object(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_vspace(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_vspace(@self, value);
end;

procedure TWebKitDOMHTMLAppletElement.set_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_applet_element_set_width(@self, value);
end;

function TWebKitDOMHTMLAreaElement.get_access_key: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_access_key(@self);
end;

function TWebKitDOMHTMLAreaElement.get_alt: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_alt(@self);
end;

function TWebKitDOMHTMLAreaElement.get_coords: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_coords(@self);
end;

function TWebKitDOMHTMLAreaElement.get_hash: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_hash(@self);
end;

function TWebKitDOMHTMLAreaElement.get_host: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_host(@self);
end;

function TWebKitDOMHTMLAreaElement.get_hostname: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_hostname(@self);
end;

function TWebKitDOMHTMLAreaElement.get_href: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_href(@self);
end;

function TWebKitDOMHTMLAreaElement.get_no_href: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_no_href(@self);
end;

function TWebKitDOMHTMLAreaElement.get_pathname: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_pathname(@self);
end;

function TWebKitDOMHTMLAreaElement.get_port: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_port(@self);
end;

function TWebKitDOMHTMLAreaElement.get_protocol: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_protocol(@self);
end;

function TWebKitDOMHTMLAreaElement.get_search: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_search(@self);
end;

function TWebKitDOMHTMLAreaElement.get_shape: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_shape(@self);
end;

function TWebKitDOMHTMLAreaElement.get_target: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_area_element_get_target(@self);
end;

procedure TWebKitDOMHTMLAreaElement.set_access_key(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_area_element_set_access_key(@self, value);
end;

procedure TWebKitDOMHTMLAreaElement.set_alt(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_area_element_set_alt(@self, value);
end;

procedure TWebKitDOMHTMLAreaElement.set_coords(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_area_element_set_coords(@self, value);
end;

procedure TWebKitDOMHTMLAreaElement.set_href(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_area_element_set_href(@self, value);
end;

procedure TWebKitDOMHTMLAreaElement.set_no_href(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_area_element_set_no_href(@self, value);
end;

procedure TWebKitDOMHTMLAreaElement.set_shape(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_area_element_set_shape(@self, value);
end;

procedure TWebKitDOMHTMLAreaElement.set_target(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_area_element_set_target(@self, value);
end;

function TWebKitDOMHTMLMediaElement.can_play_type(type_: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_can_play_type(@self, type_);
end;

function TWebKitDOMHTMLMediaElement.get_autoplay: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_autoplay(@self);
end;

function TWebKitDOMHTMLMediaElement.get_buffered: PWebKitDOMTimeRanges; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_buffered(@self);
end;

function TWebKitDOMHTMLMediaElement.get_controls: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_controls(@self);
end;

function TWebKitDOMHTMLMediaElement.get_current_src: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_current_src(@self);
end;

function TWebKitDOMHTMLMediaElement.get_current_time: gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_current_time(@self);
end;

function TWebKitDOMHTMLMediaElement.get_default_playback_rate: gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_default_playback_rate(@self);
end;

function TWebKitDOMHTMLMediaElement.get_duration: gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_duration(@self);
end;

function TWebKitDOMHTMLMediaElement.get_ended: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_ended(@self);
end;

function TWebKitDOMHTMLMediaElement.get_error: PWebKitDOMMediaError; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_error(@self);
end;

function TWebKitDOMHTMLMediaElement.get_loop: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_loop(@self);
end;

function TWebKitDOMHTMLMediaElement.get_muted: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_muted(@self);
end;

function TWebKitDOMHTMLMediaElement.get_network_state: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_network_state(@self);
end;

function TWebKitDOMHTMLMediaElement.get_paused: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_paused(@self);
end;

function TWebKitDOMHTMLMediaElement.get_playback_rate: gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_playback_rate(@self);
end;

function TWebKitDOMHTMLMediaElement.get_played: PWebKitDOMTimeRanges; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_played(@self);
end;

function TWebKitDOMHTMLMediaElement.get_preload: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_preload(@self);
end;

function TWebKitDOMHTMLMediaElement.get_ready_state: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_ready_state(@self);
end;

function TWebKitDOMHTMLMediaElement.get_seekable: PWebKitDOMTimeRanges; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_seekable(@self);
end;

function TWebKitDOMHTMLMediaElement.get_seeking: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_seeking(@self);
end;

function TWebKitDOMHTMLMediaElement.get_src: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_src(@self);
end;

function TWebKitDOMHTMLMediaElement.get_start_time: gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_start_time(@self);
end;

function TWebKitDOMHTMLMediaElement.get_volume: gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_volume(@self);
end;

function TWebKitDOMHTMLMediaElement.get_webkit_closed_captions_visible: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_webkit_closed_captions_visible(@self);
end;

function TWebKitDOMHTMLMediaElement.get_webkit_has_closed_captions: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_webkit_has_closed_captions(@self);
end;

function TWebKitDOMHTMLMediaElement.get_webkit_preserves_pitch: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_media_element_get_webkit_preserves_pitch(@self);
end;

procedure TWebKitDOMHTMLMediaElement.load(isUserGesture: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_load(@self, isUserGesture);
end;

procedure TWebKitDOMHTMLMediaElement.pause(isUserGesture: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_pause(@self, isUserGesture);
end;

procedure TWebKitDOMHTMLMediaElement.play(isUserGesture: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_play(@self, isUserGesture);
end;

procedure TWebKitDOMHTMLMediaElement.set_autoplay(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_autoplay(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_controls(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_controls(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_current_time(value: gfloat); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_current_time(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_default_playback_rate(value: gfloat); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_default_playback_rate(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_loop(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_loop(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_muted(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_muted(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_playback_rate(value: gfloat); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_playback_rate(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_preload(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_preload(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_src(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_src(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_volume(value: gfloat); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_volume(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_webkit_closed_captions_visible(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_webkit_closed_captions_visible(@self, value);
end;

procedure TWebKitDOMHTMLMediaElement.set_webkit_preserves_pitch(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_media_element_set_webkit_preserves_pitch(@self, value);
end;

function TWebKitDOMHTMLBRElement.get_clear: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlbr_element_get_clear(@self);
end;

procedure TWebKitDOMHTMLBRElement.set_clear(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_htmlbr_element_set_clear(@self, value);
end;

function TWebKitDOMHTMLBaseElement.get_href: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_base_element_get_href(@self);
end;

function TWebKitDOMHTMLBaseElement.get_target: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_base_element_get_target(@self);
end;

procedure TWebKitDOMHTMLBaseElement.set_href(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_base_element_set_href(@self, value);
end;

procedure TWebKitDOMHTMLBaseElement.set_target(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_base_element_set_target(@self, value);
end;

function TWebKitDOMHTMLBaseFontElement.get_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_base_font_element_get_color(@self);
end;

function TWebKitDOMHTMLBaseFontElement.get_face: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_base_font_element_get_face(@self);
end;

function TWebKitDOMHTMLBaseFontElement.get_size: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_base_font_element_get_size(@self);
end;

procedure TWebKitDOMHTMLBaseFontElement.set_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_base_font_element_set_color(@self, value);
end;

procedure TWebKitDOMHTMLBaseFontElement.set_face(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_base_font_element_set_face(@self, value);
end;

procedure TWebKitDOMHTMLBaseFontElement.set_size(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_base_font_element_set_size(@self, value);
end;

function TWebKitDOMHTMLBlockquoteElement.get_cite: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_blockquote_element_get_cite(@self);
end;

procedure TWebKitDOMHTMLBlockquoteElement.set_cite(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_blockquote_element_set_cite(@self, value);
end;

function TWebKitDOMHTMLBodyElement.get_a_link: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_body_element_get_a_link(@self);
end;

function TWebKitDOMHTMLBodyElement.get_background: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_body_element_get_background(@self);
end;

function TWebKitDOMHTMLBodyElement.get_bg_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_body_element_get_bg_color(@self);
end;

function TWebKitDOMHTMLBodyElement.get_link: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_body_element_get_link(@self);
end;

function TWebKitDOMHTMLBodyElement.get_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_body_element_get_text(@self);
end;

function TWebKitDOMHTMLBodyElement.get_v_link: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_body_element_get_v_link(@self);
end;

procedure TWebKitDOMHTMLBodyElement.set_a_link(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_body_element_set_a_link(@self, value);
end;

procedure TWebKitDOMHTMLBodyElement.set_background(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_body_element_set_background(@self, value);
end;

procedure TWebKitDOMHTMLBodyElement.set_bg_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_body_element_set_bg_color(@self, value);
end;

procedure TWebKitDOMHTMLBodyElement.set_link(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_body_element_set_link(@self, value);
end;

procedure TWebKitDOMHTMLBodyElement.set_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_body_element_set_text(@self, value);
end;

procedure TWebKitDOMHTMLBodyElement.set_v_link(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_body_element_set_v_link(@self, value);
end;

function TWebKitDOMHTMLFormElement.check_validity: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_check_validity(@self);
end;

procedure TWebKitDOMHTMLFormElement.dispatch_form_change; cdecl;
begin
  WebKit3.webkit_dom_html_form_element_dispatch_form_change(@self);
end;

procedure TWebKitDOMHTMLFormElement.dispatch_form_input; cdecl;
begin
  WebKit3.webkit_dom_html_form_element_dispatch_form_input(@self);
end;

function TWebKitDOMHTMLFormElement.get_accept_charset: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_accept_charset(@self);
end;

function TWebKitDOMHTMLFormElement.get_action: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_action(@self);
end;

function TWebKitDOMHTMLFormElement.get_elements: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_elements(@self);
end;

function TWebKitDOMHTMLFormElement.get_encoding: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_encoding(@self);
end;

function TWebKitDOMHTMLFormElement.get_enctype: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_enctype(@self);
end;

function TWebKitDOMHTMLFormElement.get_length: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_length(@self);
end;

function TWebKitDOMHTMLFormElement.get_method: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_method(@self);
end;

function TWebKitDOMHTMLFormElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_name(@self);
end;

function TWebKitDOMHTMLFormElement.get_no_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_no_validate(@self);
end;

function TWebKitDOMHTMLFormElement.get_target: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_form_element_get_target(@self);
end;

procedure TWebKitDOMHTMLFormElement.reset; cdecl;
begin
  WebKit3.webkit_dom_html_form_element_reset(@self);
end;

procedure TWebKitDOMHTMLFormElement.set_accept_charset(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_form_element_set_accept_charset(@self, value);
end;

procedure TWebKitDOMHTMLFormElement.set_action(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_form_element_set_action(@self, value);
end;

procedure TWebKitDOMHTMLFormElement.set_encoding(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_form_element_set_encoding(@self, value);
end;

procedure TWebKitDOMHTMLFormElement.set_enctype(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_form_element_set_enctype(@self, value);
end;

procedure TWebKitDOMHTMLFormElement.set_method(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_form_element_set_method(@self, value);
end;

procedure TWebKitDOMHTMLFormElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_form_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLFormElement.set_no_validate(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_form_element_set_no_validate(@self, value);
end;

procedure TWebKitDOMHTMLFormElement.set_target(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_form_element_set_target(@self, value);
end;

procedure TWebKitDOMHTMLFormElement.submit; cdecl;
begin
  WebKit3.webkit_dom_html_form_element_submit(@self);
end;

function TWebKitDOMValidityState.get_custom_error: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_custom_error(@self);
end;

function TWebKitDOMValidityState.get_pattern_mismatch: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_pattern_mismatch(@self);
end;

function TWebKitDOMValidityState.get_range_overflow: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_range_overflow(@self);
end;

function TWebKitDOMValidityState.get_range_underflow: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_range_underflow(@self);
end;

function TWebKitDOMValidityState.get_step_mismatch: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_step_mismatch(@self);
end;

function TWebKitDOMValidityState.get_too_long: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_too_long(@self);
end;

function TWebKitDOMValidityState.get_type_mismatch: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_type_mismatch(@self);
end;

function TWebKitDOMValidityState.get_valid: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_valid(@self);
end;

function TWebKitDOMValidityState.get_value_missing: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_validity_state_get_value_missing(@self);
end;

function TWebKitDOMHTMLButtonElement.check_validity: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_check_validity(@self);
end;

procedure TWebKitDOMHTMLButtonElement.click; cdecl;
begin
  WebKit3.webkit_dom_html_button_element_click(@self);
end;

function TWebKitDOMHTMLButtonElement.get_access_key: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_access_key(@self);
end;

function TWebKitDOMHTMLButtonElement.get_autofocus: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_autofocus(@self);
end;

function TWebKitDOMHTMLButtonElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_disabled(@self);
end;

function TWebKitDOMHTMLButtonElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_form(@self);
end;

function TWebKitDOMHTMLButtonElement.get_form_action: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_form_action(@self);
end;

function TWebKitDOMHTMLButtonElement.get_form_enctype: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_form_enctype(@self);
end;

function TWebKitDOMHTMLButtonElement.get_form_method: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_form_method(@self);
end;

function TWebKitDOMHTMLButtonElement.get_form_no_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_form_no_validate(@self);
end;

function TWebKitDOMHTMLButtonElement.get_form_target: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_form_target(@self);
end;

function TWebKitDOMHTMLButtonElement.get_labels: PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_labels(@self);
end;

function TWebKitDOMHTMLButtonElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_name(@self);
end;

function TWebKitDOMHTMLButtonElement.get_validation_message: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_validation_message(@self);
end;

function TWebKitDOMHTMLButtonElement.get_validity: PWebKitDOMValidityState; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_validity(@self);
end;

function TWebKitDOMHTMLButtonElement.get_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_value(@self);
end;

function TWebKitDOMHTMLButtonElement.get_will_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_button_element_get_will_validate(@self);
end;

procedure TWebKitDOMHTMLButtonElement.set_access_key(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_access_key(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_autofocus(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_autofocus(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_custom_validity(error: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_custom_validity(@self, error);
end;

procedure TWebKitDOMHTMLButtonElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_form_action(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_form_action(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_form_enctype(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_form_enctype(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_form_method(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_form_method(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_form_no_validate(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_form_no_validate(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_form_target(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_form_target(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLButtonElement.set_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_button_element_set_value(@self, value);
end;

function TWebKitDOMHTMLCanvasElement.get_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_canvas_element_get_height(@self);
end;

function TWebKitDOMHTMLCanvasElement.get_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_canvas_element_get_width(@self);
end;

procedure TWebKitDOMHTMLCanvasElement.set_height(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_canvas_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLCanvasElement.set_width(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_canvas_element_set_width(@self, value);
end;

function TWebKitDOMHTMLDListElement.get_compact: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_htmld_list_element_get_compact(@self);
end;

procedure TWebKitDOMHTMLDListElement.set_compact(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_htmld_list_element_set_compact(@self, value);
end;

function TWebKitDOMHTMLDetailsElement.get_open: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_details_element_get_open(@self);
end;

procedure TWebKitDOMHTMLDetailsElement.set_open(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_details_element_set_open(@self, value);
end;

function TWebKitDOMHTMLDirectoryElement.get_compact: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_directory_element_get_compact(@self);
end;

procedure TWebKitDOMHTMLDirectoryElement.set_compact(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_directory_element_set_compact(@self, value);
end;

function TWebKitDOMHTMLDivElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_div_element_get_align(@self);
end;

procedure TWebKitDOMHTMLDivElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_div_element_set_align(@self, value);
end;

function TWebKitDOMHTMLEmbedElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_embed_element_get_align(@self);
end;

function TWebKitDOMHTMLEmbedElement.get_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_embed_element_get_height(@self);
end;

function TWebKitDOMHTMLEmbedElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_embed_element_get_name(@self);
end;

function TWebKitDOMHTMLEmbedElement.get_src: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_embed_element_get_src(@self);
end;

function TWebKitDOMHTMLEmbedElement.get_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_embed_element_get_width(@self);
end;

procedure TWebKitDOMHTMLEmbedElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_embed_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLEmbedElement.set_height(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_embed_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLEmbedElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_embed_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLEmbedElement.set_src(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_embed_element_set_src(@self, value);
end;

procedure TWebKitDOMHTMLEmbedElement.set_width(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_embed_element_set_width(@self, value);
end;

function TWebKitDOMHTMLFieldSetElement.check_validity: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_field_set_element_check_validity(@self);
end;

function TWebKitDOMHTMLFieldSetElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_field_set_element_get_form(@self);
end;

function TWebKitDOMHTMLFieldSetElement.get_validation_message: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_field_set_element_get_validation_message(@self);
end;

function TWebKitDOMHTMLFieldSetElement.get_validity: PWebKitDOMValidityState; cdecl;
begin
  Result := WebKit3.webkit_dom_html_field_set_element_get_validity(@self);
end;

function TWebKitDOMHTMLFieldSetElement.get_will_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_field_set_element_get_will_validate(@self);
end;

procedure TWebKitDOMHTMLFieldSetElement.set_custom_validity(error: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_field_set_element_set_custom_validity(@self, error);
end;

function TWebKitDOMHTMLFontElement.get_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_font_element_get_color(@self);
end;

function TWebKitDOMHTMLFontElement.get_face: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_font_element_get_face(@self);
end;

function TWebKitDOMHTMLFontElement.get_size: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_font_element_get_size(@self);
end;

procedure TWebKitDOMHTMLFontElement.set_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_font_element_set_color(@self, value);
end;

procedure TWebKitDOMHTMLFontElement.set_face(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_font_element_set_face(@self, value);
end;

procedure TWebKitDOMHTMLFontElement.set_size(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_font_element_set_size(@self, value);
end;

function TWebKitDOMHTMLFrameElement.get_content_document: PWebKitDOMDocument; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_content_document(@self);
end;

function TWebKitDOMHTMLFrameElement.get_content_window: PWebKitDOMDOMWindow; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_content_window(@self);
end;

function TWebKitDOMHTMLFrameElement.get_frame_border: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_frame_border(@self);
end;

function TWebKitDOMHTMLFrameElement.get_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_height(@self);
end;

function TWebKitDOMHTMLFrameElement.get_long_desc: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_long_desc(@self);
end;

function TWebKitDOMHTMLFrameElement.get_margin_height: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_margin_height(@self);
end;

function TWebKitDOMHTMLFrameElement.get_margin_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_margin_width(@self);
end;

function TWebKitDOMHTMLFrameElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_name(@self);
end;

function TWebKitDOMHTMLFrameElement.get_no_resize: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_no_resize(@self);
end;

function TWebKitDOMHTMLFrameElement.get_scrolling: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_scrolling(@self);
end;

function TWebKitDOMHTMLFrameElement.get_src: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_src(@self);
end;

function TWebKitDOMHTMLFrameElement.get_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_element_get_width(@self);
end;

procedure TWebKitDOMHTMLFrameElement.set_frame_border(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_element_set_frame_border(@self, value);
end;

procedure TWebKitDOMHTMLFrameElement.set_long_desc(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_element_set_long_desc(@self, value);
end;

procedure TWebKitDOMHTMLFrameElement.set_margin_height(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_element_set_margin_height(@self, value);
end;

procedure TWebKitDOMHTMLFrameElement.set_margin_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_element_set_margin_width(@self, value);
end;

procedure TWebKitDOMHTMLFrameElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLFrameElement.set_no_resize(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_frame_element_set_no_resize(@self, value);
end;

procedure TWebKitDOMHTMLFrameElement.set_scrolling(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_element_set_scrolling(@self, value);
end;

procedure TWebKitDOMHTMLFrameElement.set_src(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_element_set_src(@self, value);
end;

function TWebKitDOMHTMLFrameSetElement.get_cols: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_set_element_get_cols(@self);
end;

function TWebKitDOMHTMLFrameSetElement.get_rows: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_frame_set_element_get_rows(@self);
end;

procedure TWebKitDOMHTMLFrameSetElement.set_cols(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_set_element_set_cols(@self, value);
end;

procedure TWebKitDOMHTMLFrameSetElement.set_rows(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_frame_set_element_set_rows(@self, value);
end;

function TWebKitDOMHTMLHRElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlhr_element_get_align(@self);
end;

function TWebKitDOMHTMLHRElement.get_no_shade: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlhr_element_get_no_shade(@self);
end;

function TWebKitDOMHTMLHRElement.get_size: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlhr_element_get_size(@self);
end;

function TWebKitDOMHTMLHRElement.get_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlhr_element_get_width(@self);
end;

procedure TWebKitDOMHTMLHRElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_htmlhr_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLHRElement.set_no_shade(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_htmlhr_element_set_no_shade(@self, value);
end;

procedure TWebKitDOMHTMLHRElement.set_size(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_htmlhr_element_set_size(@self, value);
end;

procedure TWebKitDOMHTMLHRElement.set_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_htmlhr_element_set_width(@self, value);
end;

function TWebKitDOMHTMLHeadingElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_heading_element_get_align(@self);
end;

procedure TWebKitDOMHTMLHeadingElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_heading_element_set_align(@self, value);
end;

function TWebKitDOMHTMLHtmlElement.get_manifest: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_html_element_get_manifest(@self);
end;

function TWebKitDOMHTMLHtmlElement.get_version: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_html_element_get_version(@self);
end;

procedure TWebKitDOMHTMLHtmlElement.set_manifest(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_html_element_set_manifest(@self, value);
end;

procedure TWebKitDOMHTMLHtmlElement.set_version(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_html_element_set_version(@self, value);
end;

function TWebKitDOMHTMLIFrameElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_align(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_content_document: PWebKitDOMDocument; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_content_document(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_content_window: PWebKitDOMDOMWindow; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_content_window(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_frame_border: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_frame_border(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_height: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_height(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_long_desc: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_long_desc(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_margin_height: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_margin_height(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_margin_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_margin_width(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_name(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_sandbox: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_sandbox(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_scrolling: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_scrolling(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_src: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_src(@self);
end;

function TWebKitDOMHTMLIFrameElement.get_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_iframe_element_get_width(@self);
end;

procedure TWebKitDOMHTMLIFrameElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_frame_border(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_frame_border(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_height(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_long_desc(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_long_desc(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_margin_height(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_margin_height(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_margin_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_margin_width(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_sandbox(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_sandbox(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_scrolling(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_scrolling(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_src(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_src(@self, value);
end;

procedure TWebKitDOMHTMLIFrameElement.set_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_iframe_element_set_width(@self, value);
end;

function TWebKitDOMHTMLImageElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_align(@self);
end;

function TWebKitDOMHTMLImageElement.get_alt: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_alt(@self);
end;

function TWebKitDOMHTMLImageElement.get_border: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_border(@self);
end;

function TWebKitDOMHTMLImageElement.get_complete: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_complete(@self);
end;

function TWebKitDOMHTMLImageElement.get_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_height(@self);
end;

function TWebKitDOMHTMLImageElement.get_hspace: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_hspace(@self);
end;

function TWebKitDOMHTMLImageElement.get_is_map: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_is_map(@self);
end;

function TWebKitDOMHTMLImageElement.get_long_desc: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_long_desc(@self);
end;

function TWebKitDOMHTMLImageElement.get_lowsrc: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_lowsrc(@self);
end;

function TWebKitDOMHTMLImageElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_name(@self);
end;

function TWebKitDOMHTMLImageElement.get_natural_height: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_natural_height(@self);
end;

function TWebKitDOMHTMLImageElement.get_natural_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_natural_width(@self);
end;

function TWebKitDOMHTMLImageElement.get_src: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_src(@self);
end;

function TWebKitDOMHTMLImageElement.get_use_map: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_use_map(@self);
end;

function TWebKitDOMHTMLImageElement.get_vspace: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_vspace(@self);
end;

function TWebKitDOMHTMLImageElement.get_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_width(@self);
end;

function TWebKitDOMHTMLImageElement.get_x: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_x(@self);
end;

function TWebKitDOMHTMLImageElement.get_y: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_image_element_get_y(@self);
end;

procedure TWebKitDOMHTMLImageElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_alt(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_alt(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_border(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_border(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_height(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_hspace(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_hspace(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_is_map(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_is_map(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_long_desc(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_long_desc(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_lowsrc(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_lowsrc(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_src(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_src(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_use_map(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_use_map(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_vspace(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_vspace(@self, value);
end;

procedure TWebKitDOMHTMLImageElement.set_width(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_image_element_set_width(@self, value);
end;

function TWebKitDOMHTMLOptionElement.get_default_selected: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_option_element_get_default_selected(@self);
end;

function TWebKitDOMHTMLOptionElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_option_element_get_disabled(@self);
end;

function TWebKitDOMHTMLOptionElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_option_element_get_form(@self);
end;

function TWebKitDOMHTMLOptionElement.get_index: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_option_element_get_index(@self);
end;

function TWebKitDOMHTMLOptionElement.get_label: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_option_element_get_label(@self);
end;

function TWebKitDOMHTMLOptionElement.get_selected: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_option_element_get_selected(@self);
end;

function TWebKitDOMHTMLOptionElement.get_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_option_element_get_text(@self);
end;

function TWebKitDOMHTMLOptionElement.get_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_option_element_get_value(@self);
end;

procedure TWebKitDOMHTMLOptionElement.set_default_selected(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_option_element_set_default_selected(@self, value);
end;

procedure TWebKitDOMHTMLOptionElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_option_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLOptionElement.set_label(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_option_element_set_label(@self, value);
end;

procedure TWebKitDOMHTMLOptionElement.set_selected(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_option_element_set_selected(@self, value);
end;

procedure TWebKitDOMHTMLOptionElement.set_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_option_element_set_value(@self, value);
end;

function TWebKitDOMHTMLInputElement.check_validity: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_check_validity(@self);
end;

procedure TWebKitDOMHTMLInputElement.click; cdecl;
begin
  WebKit3.webkit_dom_html_input_element_click(@self);
end;

function TWebKitDOMHTMLInputElement.get_accept: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_accept(@self);
end;

function TWebKitDOMHTMLInputElement.get_access_key: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_access_key(@self);
end;

function TWebKitDOMHTMLInputElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_align(@self);
end;

function TWebKitDOMHTMLInputElement.get_alt: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_alt(@self);
end;

function TWebKitDOMHTMLInputElement.get_autofocus: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_autofocus(@self);
end;

function TWebKitDOMHTMLInputElement.get_checked: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_checked(@self);
end;

function TWebKitDOMHTMLInputElement.get_default_checked: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_default_checked(@self);
end;

function TWebKitDOMHTMLInputElement.get_default_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_default_value(@self);
end;

function TWebKitDOMHTMLInputElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_disabled(@self);
end;

function TWebKitDOMHTMLInputElement.get_files: PWebKitDOMFileList; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_files(@self);
end;

function TWebKitDOMHTMLInputElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_form(@self);
end;

function TWebKitDOMHTMLInputElement.get_form_action: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_form_action(@self);
end;

function TWebKitDOMHTMLInputElement.get_form_enctype: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_form_enctype(@self);
end;

function TWebKitDOMHTMLInputElement.get_form_method: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_form_method(@self);
end;

function TWebKitDOMHTMLInputElement.get_form_no_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_form_no_validate(@self);
end;

function TWebKitDOMHTMLInputElement.get_form_target: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_form_target(@self);
end;

function TWebKitDOMHTMLInputElement.get_incremental: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_incremental(@self);
end;

function TWebKitDOMHTMLInputElement.get_indeterminate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_indeterminate(@self);
end;

function TWebKitDOMHTMLInputElement.get_labels: PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_labels(@self);
end;

function TWebKitDOMHTMLInputElement.get_list: PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_list(@self);
end;

function TWebKitDOMHTMLInputElement.get_max: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_max(@self);
end;

function TWebKitDOMHTMLInputElement.get_max_length: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_max_length(@self);
end;

function TWebKitDOMHTMLInputElement.get_min: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_min(@self);
end;

function TWebKitDOMHTMLInputElement.get_multiple: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_multiple(@self);
end;

function TWebKitDOMHTMLInputElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_name(@self);
end;

function TWebKitDOMHTMLInputElement.get_pattern: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_pattern(@self);
end;

function TWebKitDOMHTMLInputElement.get_placeholder: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_placeholder(@self);
end;

function TWebKitDOMHTMLInputElement.get_read_only: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_read_only(@self);
end;

function TWebKitDOMHTMLInputElement.get_required: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_required(@self);
end;

function TWebKitDOMHTMLInputElement.get_selected_option: PWebKitDOMHTMLOptionElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_selected_option(@self);
end;

function TWebKitDOMHTMLInputElement.get_size: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_size(@self);
end;

function TWebKitDOMHTMLInputElement.get_src: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_src(@self);
end;

function TWebKitDOMHTMLInputElement.get_step: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_step(@self);
end;

function TWebKitDOMHTMLInputElement.get_use_map: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_use_map(@self);
end;

function TWebKitDOMHTMLInputElement.get_validation_message: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_validation_message(@self);
end;

function TWebKitDOMHTMLInputElement.get_validity: PWebKitDOMValidityState; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_validity(@self);
end;

function TWebKitDOMHTMLInputElement.get_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_value(@self);
end;

function TWebKitDOMHTMLInputElement.get_value_as_number: gdouble; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_value_as_number(@self);
end;

function TWebKitDOMHTMLInputElement.get_will_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_input_element_get_will_validate(@self);
end;

procedure TWebKitDOMHTMLInputElement.select; cdecl;
begin
  WebKit3.webkit_dom_html_input_element_select(@self);
end;

procedure TWebKitDOMHTMLInputElement.set_accept(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_accept(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_access_key(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_access_key(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_alt(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_alt(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_autofocus(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_autofocus(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_checked(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_checked(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_custom_validity(error: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_custom_validity(@self, error);
end;

procedure TWebKitDOMHTMLInputElement.set_default_checked(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_default_checked(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_default_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_default_value(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_form_action(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_form_action(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_form_enctype(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_form_enctype(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_form_method(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_form_method(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_form_no_validate(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_form_no_validate(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_form_target(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_form_target(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_incremental(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_incremental(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_indeterminate(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_indeterminate(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_max(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_max(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_max_length(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_max_length(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_min(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_min(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_multiple(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_multiple(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_pattern(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_pattern(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_placeholder(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_placeholder(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_read_only(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_read_only(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_required(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_required(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_size(value: gulong); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_size(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_src(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_src(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_step(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_step(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_use_map(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_use_map(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_value(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_value_as_number(value: gdouble); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_value_as_number(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.set_value_for_user(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_set_value_for_user(@self, value);
end;

procedure TWebKitDOMHTMLInputElement.step_down(n: glong); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_step_down(@self, n);
end;

procedure TWebKitDOMHTMLInputElement.step_up(n: glong); cdecl;
begin
  WebKit3.webkit_dom_html_input_element_step_up(@self, n);
end;

function TWebKitDOMHTMLIsIndexElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_is_index_element_get_form(@self);
end;

function TWebKitDOMHTMLIsIndexElement.get_prompt: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_is_index_element_get_prompt(@self);
end;

procedure TWebKitDOMHTMLIsIndexElement.set_prompt(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_is_index_element_set_prompt(@self, value);
end;

function TWebKitDOMHTMLKeygenElement.check_validity: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_check_validity(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_autofocus: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_autofocus(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_challenge: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_challenge(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_disabled(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_form(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_keytype: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_keytype(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_labels: PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_labels(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_name(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_validation_message: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_validation_message(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_validity: PWebKitDOMValidityState; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_validity(@self);
end;

function TWebKitDOMHTMLKeygenElement.get_will_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_keygen_element_get_will_validate(@self);
end;

procedure TWebKitDOMHTMLKeygenElement.set_autofocus(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_keygen_element_set_autofocus(@self, value);
end;

procedure TWebKitDOMHTMLKeygenElement.set_challenge(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_keygen_element_set_challenge(@self, value);
end;

procedure TWebKitDOMHTMLKeygenElement.set_custom_validity(error: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_keygen_element_set_custom_validity(@self, error);
end;

procedure TWebKitDOMHTMLKeygenElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_keygen_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLKeygenElement.set_keytype(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_keygen_element_set_keytype(@self, value);
end;

procedure TWebKitDOMHTMLKeygenElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_keygen_element_set_name(@self, value);
end;

function TWebKitDOMHTMLLIElement.get_value: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlli_element_get_value(@self);
end;

procedure TWebKitDOMHTMLLIElement.set_value(value: glong); cdecl;
begin
  WebKit3.webkit_dom_htmlli_element_set_value(@self, value);
end;

function TWebKitDOMHTMLLabelElement.get_access_key: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_label_element_get_access_key(@self);
end;

function TWebKitDOMHTMLLabelElement.get_control: PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_label_element_get_control(@self);
end;

function TWebKitDOMHTMLLabelElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_label_element_get_form(@self);
end;

function TWebKitDOMHTMLLabelElement.get_html_for: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_label_element_get_html_for(@self);
end;

procedure TWebKitDOMHTMLLabelElement.set_access_key(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_label_element_set_access_key(@self, value);
end;

procedure TWebKitDOMHTMLLabelElement.set_html_for(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_label_element_set_html_for(@self, value);
end;

function TWebKitDOMHTMLLegendElement.get_access_key: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_legend_element_get_access_key(@self);
end;

function TWebKitDOMHTMLLegendElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_legend_element_get_align(@self);
end;

function TWebKitDOMHTMLLegendElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_legend_element_get_form(@self);
end;

procedure TWebKitDOMHTMLLegendElement.set_access_key(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_legend_element_set_access_key(@self, value);
end;

procedure TWebKitDOMHTMLLegendElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_legend_element_set_align(@self, value);
end;

function TWebKitDOMHTMLLinkElement.get_charset: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_charset(@self);
end;

function TWebKitDOMHTMLLinkElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_disabled(@self);
end;

function TWebKitDOMHTMLLinkElement.get_href: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_href(@self);
end;

function TWebKitDOMHTMLLinkElement.get_hreflang: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_hreflang(@self);
end;

function TWebKitDOMHTMLLinkElement.get_media: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_media(@self);
end;

function TWebKitDOMHTMLLinkElement.get_rel: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_rel(@self);
end;

function TWebKitDOMHTMLLinkElement.get_rev: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_rev(@self);
end;

function TWebKitDOMHTMLLinkElement.get_sheet: PWebKitDOMStyleSheet; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_sheet(@self);
end;

function TWebKitDOMHTMLLinkElement.get_target: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_link_element_get_target(@self);
end;

procedure TWebKitDOMHTMLLinkElement.set_charset(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_link_element_set_charset(@self, value);
end;

procedure TWebKitDOMHTMLLinkElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_link_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLLinkElement.set_href(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_link_element_set_href(@self, value);
end;

procedure TWebKitDOMHTMLLinkElement.set_hreflang(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_link_element_set_hreflang(@self, value);
end;

procedure TWebKitDOMHTMLLinkElement.set_media(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_link_element_set_media(@self, value);
end;

procedure TWebKitDOMHTMLLinkElement.set_rel(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_link_element_set_rel(@self, value);
end;

procedure TWebKitDOMHTMLLinkElement.set_rev(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_link_element_set_rev(@self, value);
end;

procedure TWebKitDOMHTMLLinkElement.set_target(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_link_element_set_target(@self, value);
end;

function TWebKitDOMHTMLMapElement.get_areas: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_map_element_get_areas(@self);
end;

function TWebKitDOMHTMLMapElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_map_element_get_name(@self);
end;

procedure TWebKitDOMHTMLMapElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_map_element_set_name(@self, value);
end;

function TWebKitDOMHTMLMarqueeElement.get_behavior: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_behavior(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_bg_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_bg_color(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_direction: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_direction(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_height: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_height(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_hspace: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_hspace(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_loop: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_loop(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_scroll_amount: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_scroll_amount(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_scroll_delay: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_scroll_delay(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_true_speed: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_true_speed(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_vspace: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_vspace(@self);
end;

function TWebKitDOMHTMLMarqueeElement.get_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_marquee_element_get_width(@self);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_behavior(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_behavior(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_bg_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_bg_color(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_direction(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_direction(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_height(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_hspace(value: gulong); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_hspace(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_loop(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_loop(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_scroll_amount(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_scroll_amount(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_scroll_delay(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_scroll_delay(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_true_speed(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_true_speed(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_vspace(value: gulong); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_vspace(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.set_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_set_width(@self, value);
end;

procedure TWebKitDOMHTMLMarqueeElement.start; cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_start(@self);
end;

procedure TWebKitDOMHTMLMarqueeElement.stop; cdecl;
begin
  WebKit3.webkit_dom_html_marquee_element_stop(@self);
end;

function TWebKitDOMTimeRanges.end_(index: gulong): gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_time_ranges_end(@self, index);
end;

function TWebKitDOMTimeRanges.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_time_ranges_get_length(@self);
end;

function TWebKitDOMTimeRanges.start(index: gulong): gfloat; cdecl;
begin
  Result := WebKit3.webkit_dom_time_ranges_start(@self, index);
end;

function TWebKitDOMMediaError.get_code: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_media_error_get_code(@self);
end;

function TWebKitDOMHTMLMenuElement.get_compact: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_menu_element_get_compact(@self);
end;

procedure TWebKitDOMHTMLMenuElement.set_compact(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_menu_element_set_compact(@self, value);
end;

function TWebKitDOMHTMLMetaElement.get_content: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_meta_element_get_content(@self);
end;

function TWebKitDOMHTMLMetaElement.get_http_equiv: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_meta_element_get_http_equiv(@self);
end;

function TWebKitDOMHTMLMetaElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_meta_element_get_name(@self);
end;

function TWebKitDOMHTMLMetaElement.get_scheme: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_meta_element_get_scheme(@self);
end;

procedure TWebKitDOMHTMLMetaElement.set_content(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_meta_element_set_content(@self, value);
end;

procedure TWebKitDOMHTMLMetaElement.set_http_equiv(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_meta_element_set_http_equiv(@self, value);
end;

procedure TWebKitDOMHTMLMetaElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_meta_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLMetaElement.set_scheme(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_meta_element_set_scheme(@self, value);
end;

function TWebKitDOMHTMLModElement.get_cite: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_mod_element_get_cite(@self);
end;

function TWebKitDOMHTMLModElement.get_date_time: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_mod_element_get_date_time(@self);
end;

procedure TWebKitDOMHTMLModElement.set_cite(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_mod_element_set_cite(@self, value);
end;

procedure TWebKitDOMHTMLModElement.set_date_time(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_mod_element_set_date_time(@self, value);
end;

function TWebKitDOMHTMLOListElement.get_compact: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlo_list_element_get_compact(@self);
end;

function TWebKitDOMHTMLOListElement.get_start: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlo_list_element_get_start(@self);
end;

procedure TWebKitDOMHTMLOListElement.set_compact(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_htmlo_list_element_set_compact(@self, value);
end;

procedure TWebKitDOMHTMLOListElement.set_start(value: glong); cdecl;
begin
  WebKit3.webkit_dom_htmlo_list_element_set_start(@self, value);
end;

function TWebKitDOMHTMLObjectElement.check_validity: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_check_validity(@self);
end;

function TWebKitDOMHTMLObjectElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_align(@self);
end;

function TWebKitDOMHTMLObjectElement.get_archive: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_archive(@self);
end;

function TWebKitDOMHTMLObjectElement.get_border: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_border(@self);
end;

function TWebKitDOMHTMLObjectElement.get_code: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_code(@self);
end;

function TWebKitDOMHTMLObjectElement.get_code_base: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_code_base(@self);
end;

function TWebKitDOMHTMLObjectElement.get_code_type: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_code_type(@self);
end;

function TWebKitDOMHTMLObjectElement.get_content_document: PWebKitDOMDocument; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_content_document(@self);
end;

function TWebKitDOMHTMLObjectElement.get_data: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_data(@self);
end;

function TWebKitDOMHTMLObjectElement.get_declare: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_declare(@self);
end;

function TWebKitDOMHTMLObjectElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_form(@self);
end;

function TWebKitDOMHTMLObjectElement.get_height: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_height(@self);
end;

function TWebKitDOMHTMLObjectElement.get_hspace: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_hspace(@self);
end;

function TWebKitDOMHTMLObjectElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_name(@self);
end;

function TWebKitDOMHTMLObjectElement.get_standby: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_standby(@self);
end;

function TWebKitDOMHTMLObjectElement.get_use_map: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_use_map(@self);
end;

function TWebKitDOMHTMLObjectElement.get_validation_message: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_validation_message(@self);
end;

function TWebKitDOMHTMLObjectElement.get_validity: PWebKitDOMValidityState; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_validity(@self);
end;

function TWebKitDOMHTMLObjectElement.get_vspace: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_vspace(@self);
end;

function TWebKitDOMHTMLObjectElement.get_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_width(@self);
end;

function TWebKitDOMHTMLObjectElement.get_will_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_object_element_get_will_validate(@self);
end;

procedure TWebKitDOMHTMLObjectElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_archive(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_archive(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_border(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_border(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_code(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_code(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_code_base(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_code_base(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_code_type(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_code_type(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_custom_validity(error: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_custom_validity(@self, error);
end;

procedure TWebKitDOMHTMLObjectElement.set_data(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_data(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_declare(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_declare(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_height(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_hspace(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_hspace(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_standby(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_standby(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_use_map(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_use_map(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_vspace(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_vspace(@self, value);
end;

procedure TWebKitDOMHTMLObjectElement.set_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_object_element_set_width(@self, value);
end;

function TWebKitDOMHTMLOptGroupElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_opt_group_element_get_disabled(@self);
end;

function TWebKitDOMHTMLOptGroupElement.get_label: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_opt_group_element_get_label(@self);
end;

procedure TWebKitDOMHTMLOptGroupElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_opt_group_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLOptGroupElement.set_label(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_opt_group_element_set_label(@self, value);
end;

function TWebKitDOMHTMLOptionsCollection.get_selected_index: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_options_collection_get_selected_index(@self);
end;

procedure TWebKitDOMHTMLOptionsCollection.set_selected_index(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_options_collection_set_selected_index(@self, value);
end;

function TWebKitDOMHTMLParagraphElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_paragraph_element_get_align(@self);
end;

procedure TWebKitDOMHTMLParagraphElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_paragraph_element_set_align(@self, value);
end;

function TWebKitDOMHTMLParamElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_param_element_get_name(@self);
end;

function TWebKitDOMHTMLParamElement.get_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_param_element_get_value(@self);
end;

function TWebKitDOMHTMLParamElement.get_value_type: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_param_element_get_value_type(@self);
end;

procedure TWebKitDOMHTMLParamElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_param_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLParamElement.set_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_param_element_set_value(@self, value);
end;

procedure TWebKitDOMHTMLParamElement.set_value_type(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_param_element_set_value_type(@self, value);
end;

function TWebKitDOMHTMLPreElement.get_width: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_pre_element_get_width(@self);
end;

function TWebKitDOMHTMLPreElement.get_wrap: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_pre_element_get_wrap(@self);
end;

procedure TWebKitDOMHTMLPreElement.set_width(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_pre_element_set_width(@self, value);
end;

procedure TWebKitDOMHTMLPreElement.set_wrap(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_pre_element_set_wrap(@self, value);
end;

function TWebKitDOMHTMLQuoteElement.get_cite: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_quote_element_get_cite(@self);
end;

procedure TWebKitDOMHTMLQuoteElement.set_cite(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_quote_element_set_cite(@self, value);
end;

function TWebKitDOMHTMLScriptElement.get_async: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_script_element_get_async(@self);
end;

function TWebKitDOMHTMLScriptElement.get_charset: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_script_element_get_charset(@self);
end;

function TWebKitDOMHTMLScriptElement.get_defer: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_script_element_get_defer(@self);
end;

function TWebKitDOMHTMLScriptElement.get_event: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_script_element_get_event(@self);
end;

function TWebKitDOMHTMLScriptElement.get_html_for: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_script_element_get_html_for(@self);
end;

function TWebKitDOMHTMLScriptElement.get_src: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_script_element_get_src(@self);
end;

function TWebKitDOMHTMLScriptElement.get_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_script_element_get_text(@self);
end;

procedure TWebKitDOMHTMLScriptElement.set_async(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_script_element_set_async(@self, value);
end;

procedure TWebKitDOMHTMLScriptElement.set_charset(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_script_element_set_charset(@self, value);
end;

procedure TWebKitDOMHTMLScriptElement.set_defer(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_script_element_set_defer(@self, value);
end;

procedure TWebKitDOMHTMLScriptElement.set_event(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_script_element_set_event(@self, value);
end;

procedure TWebKitDOMHTMLScriptElement.set_html_for(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_script_element_set_html_for(@self, value);
end;

procedure TWebKitDOMHTMLScriptElement.set_src(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_script_element_set_src(@self, value);
end;

procedure TWebKitDOMHTMLScriptElement.set_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_script_element_set_text(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.add(element: PWebKitDOMHTMLElement; before: PWebKitDOMHTMLElement); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_add(@self, element, before);
end;

function TWebKitDOMHTMLSelectElement.check_validity: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_check_validity(@self);
end;

function TWebKitDOMHTMLSelectElement.get_autofocus: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_autofocus(@self);
end;

function TWebKitDOMHTMLSelectElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_disabled(@self);
end;

function TWebKitDOMHTMLSelectElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_form(@self);
end;

function TWebKitDOMHTMLSelectElement.get_labels: PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_labels(@self);
end;

function TWebKitDOMHTMLSelectElement.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_length(@self);
end;

function TWebKitDOMHTMLSelectElement.get_multiple: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_multiple(@self);
end;

function TWebKitDOMHTMLSelectElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_name(@self);
end;

function TWebKitDOMHTMLSelectElement.get_options: PWebKitDOMHTMLOptionsCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_options(@self);
end;

function TWebKitDOMHTMLSelectElement.get_required: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_required(@self);
end;

function TWebKitDOMHTMLSelectElement.get_selected_index: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_selected_index(@self);
end;

function TWebKitDOMHTMLSelectElement.get_size: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_size(@self);
end;

function TWebKitDOMHTMLSelectElement.get_validation_message: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_validation_message(@self);
end;

function TWebKitDOMHTMLSelectElement.get_validity: PWebKitDOMValidityState; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_validity(@self);
end;

function TWebKitDOMHTMLSelectElement.get_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_value(@self);
end;

function TWebKitDOMHTMLSelectElement.get_will_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_get_will_validate(@self);
end;

function TWebKitDOMHTMLSelectElement.item(index: gulong): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_item(@self, index);
end;

function TWebKitDOMHTMLSelectElement.named_item(name: Pgchar): PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_html_select_element_named_item(@self, name);
end;

procedure TWebKitDOMHTMLSelectElement.remove(index: glong); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_remove(@self, index);
end;

procedure TWebKitDOMHTMLSelectElement.set_autofocus(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_autofocus(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.set_custom_validity(error: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_custom_validity(@self, error);
end;

procedure TWebKitDOMHTMLSelectElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.set_length(value: gulong); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_length(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.set_multiple(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_multiple(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.set_required(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_required(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.set_selected_index(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_selected_index(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.set_size(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_size(@self, value);
end;

procedure TWebKitDOMHTMLSelectElement.set_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_select_element_set_value(@self, value);
end;

function TWebKitDOMHTMLStyleElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_style_element_get_disabled(@self);
end;

function TWebKitDOMHTMLStyleElement.get_media: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_style_element_get_media(@self);
end;

function TWebKitDOMHTMLStyleElement.get_sheet: PWebKitDOMStyleSheet; cdecl;
begin
  Result := WebKit3.webkit_dom_html_style_element_get_sheet(@self);
end;

procedure TWebKitDOMHTMLStyleElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_style_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLStyleElement.set_media(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_style_element_set_media(@self, value);
end;

function TWebKitDOMHTMLTableCaptionElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_caption_element_get_align(@self);
end;

procedure TWebKitDOMHTMLTableCaptionElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_caption_element_set_align(@self, value);
end;

function TWebKitDOMHTMLTableCellElement.get_abbr: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_abbr(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_align(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_axis: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_axis(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_bg_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_bg_color(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_cell_index: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_cell_index(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_ch: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_ch(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_ch_off: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_ch_off(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_col_span: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_col_span(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_headers: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_headers(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_height: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_height(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_no_wrap: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_no_wrap(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_row_span: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_row_span(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_scope: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_scope(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_v_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_v_align(@self);
end;

function TWebKitDOMHTMLTableCellElement.get_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_cell_element_get_width(@self);
end;

procedure TWebKitDOMHTMLTableCellElement.set_abbr(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_abbr(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_axis(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_axis(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_bg_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_bg_color(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_ch(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_ch(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_ch_off(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_ch_off(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_col_span(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_col_span(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_headers(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_headers(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_height(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_no_wrap(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_no_wrap(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_row_span(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_row_span(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_scope(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_scope(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_v_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_v_align(@self, value);
end;

procedure TWebKitDOMHTMLTableCellElement.set_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_cell_element_set_width(@self, value);
end;

function TWebKitDOMHTMLTableColElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_col_element_get_align(@self);
end;

function TWebKitDOMHTMLTableColElement.get_ch: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_col_element_get_ch(@self);
end;

function TWebKitDOMHTMLTableColElement.get_ch_off: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_col_element_get_ch_off(@self);
end;

function TWebKitDOMHTMLTableColElement.get_span: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_col_element_get_span(@self);
end;

function TWebKitDOMHTMLTableColElement.get_v_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_col_element_get_v_align(@self);
end;

function TWebKitDOMHTMLTableColElement.get_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_col_element_get_width(@self);
end;

procedure TWebKitDOMHTMLTableColElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_col_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLTableColElement.set_ch(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_col_element_set_ch(@self, value);
end;

procedure TWebKitDOMHTMLTableColElement.set_ch_off(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_col_element_set_ch_off(@self, value);
end;

procedure TWebKitDOMHTMLTableColElement.set_span(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_table_col_element_set_span(@self, value);
end;

procedure TWebKitDOMHTMLTableColElement.set_v_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_col_element_set_v_align(@self, value);
end;

procedure TWebKitDOMHTMLTableColElement.set_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_col_element_set_width(@self, value);
end;

procedure TWebKitDOMHTMLTableSectionElement.delete_row(index: glong); cdecl;
begin
  WebKit3.webkit_dom_html_table_section_element_delete_row(@self, index);
end;

function TWebKitDOMHTMLTableSectionElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_section_element_get_align(@self);
end;

function TWebKitDOMHTMLTableSectionElement.get_ch: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_section_element_get_ch(@self);
end;

function TWebKitDOMHTMLTableSectionElement.get_ch_off: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_section_element_get_ch_off(@self);
end;

function TWebKitDOMHTMLTableSectionElement.get_rows: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_section_element_get_rows(@self);
end;

function TWebKitDOMHTMLTableSectionElement.get_v_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_section_element_get_v_align(@self);
end;

function TWebKitDOMHTMLTableSectionElement.insert_row(index: glong): PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_section_element_insert_row(@self, index);
end;

procedure TWebKitDOMHTMLTableSectionElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_section_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLTableSectionElement.set_ch(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_section_element_set_ch(@self, value);
end;

procedure TWebKitDOMHTMLTableSectionElement.set_ch_off(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_section_element_set_ch_off(@self, value);
end;

procedure TWebKitDOMHTMLTableSectionElement.set_v_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_section_element_set_v_align(@self, value);
end;

function TWebKitDOMHTMLTableElement.create_caption: PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_create_caption(@self);
end;

function TWebKitDOMHTMLTableElement.create_t_foot: PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_create_t_foot(@self);
end;

function TWebKitDOMHTMLTableElement.create_t_head: PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_create_t_head(@self);
end;

procedure TWebKitDOMHTMLTableElement.delete_caption; cdecl;
begin
  WebKit3.webkit_dom_html_table_element_delete_caption(@self);
end;

procedure TWebKitDOMHTMLTableElement.delete_row(index: glong); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_delete_row(@self, index);
end;

procedure TWebKitDOMHTMLTableElement.delete_t_foot; cdecl;
begin
  WebKit3.webkit_dom_html_table_element_delete_t_foot(@self);
end;

procedure TWebKitDOMHTMLTableElement.delete_t_head; cdecl;
begin
  WebKit3.webkit_dom_html_table_element_delete_t_head(@self);
end;

function TWebKitDOMHTMLTableElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_align(@self);
end;

function TWebKitDOMHTMLTableElement.get_bg_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_bg_color(@self);
end;

function TWebKitDOMHTMLTableElement.get_border: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_border(@self);
end;

function TWebKitDOMHTMLTableElement.get_caption: PWebKitDOMHTMLTableCaptionElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_caption(@self);
end;

function TWebKitDOMHTMLTableElement.get_cell_padding: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_cell_padding(@self);
end;

function TWebKitDOMHTMLTableElement.get_cell_spacing: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_cell_spacing(@self);
end;

function TWebKitDOMHTMLTableElement.get_frame: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_frame(@self);
end;

function TWebKitDOMHTMLTableElement.get_rows: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_rows(@self);
end;

function TWebKitDOMHTMLTableElement.get_rules: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_rules(@self);
end;

function TWebKitDOMHTMLTableElement.get_summary: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_summary(@self);
end;

function TWebKitDOMHTMLTableElement.get_t_bodies: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_t_bodies(@self);
end;

function TWebKitDOMHTMLTableElement.get_t_foot: PWebKitDOMHTMLTableSectionElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_t_foot(@self);
end;

function TWebKitDOMHTMLTableElement.get_t_head: PWebKitDOMHTMLTableSectionElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_t_head(@self);
end;

function TWebKitDOMHTMLTableElement.get_width: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_get_width(@self);
end;

function TWebKitDOMHTMLTableElement.insert_row(index: glong): PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_element_insert_row(@self, index);
end;

procedure TWebKitDOMHTMLTableElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_bg_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_bg_color(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_border(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_border(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_caption(value: PWebKitDOMHTMLTableCaptionElement); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_caption(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_cell_padding(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_cell_padding(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_cell_spacing(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_cell_spacing(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_frame(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_frame(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_rules(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_rules(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_summary(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_summary(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_t_foot(value: PWebKitDOMHTMLTableSectionElement); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_t_foot(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_t_head(value: PWebKitDOMHTMLTableSectionElement); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_t_head(@self, value);
end;

procedure TWebKitDOMHTMLTableElement.set_width(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_element_set_width(@self, value);
end;

procedure TWebKitDOMHTMLTableRowElement.delete_cell(index: glong); cdecl;
begin
  WebKit3.webkit_dom_html_table_row_element_delete_cell(@self, index);
end;

function TWebKitDOMHTMLTableRowElement.get_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_get_align(@self);
end;

function TWebKitDOMHTMLTableRowElement.get_bg_color: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_get_bg_color(@self);
end;

function TWebKitDOMHTMLTableRowElement.get_cells: PWebKitDOMHTMLCollection; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_get_cells(@self);
end;

function TWebKitDOMHTMLTableRowElement.get_ch: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_get_ch(@self);
end;

function TWebKitDOMHTMLTableRowElement.get_ch_off: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_get_ch_off(@self);
end;

function TWebKitDOMHTMLTableRowElement.get_row_index: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_get_row_index(@self);
end;

function TWebKitDOMHTMLTableRowElement.get_section_row_index: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_get_section_row_index(@self);
end;

function TWebKitDOMHTMLTableRowElement.get_v_align: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_get_v_align(@self);
end;

function TWebKitDOMHTMLTableRowElement.insert_cell(index: glong): PWebKitDOMHTMLElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_table_row_element_insert_cell(@self, index);
end;

procedure TWebKitDOMHTMLTableRowElement.set_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_row_element_set_align(@self, value);
end;

procedure TWebKitDOMHTMLTableRowElement.set_bg_color(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_row_element_set_bg_color(@self, value);
end;

procedure TWebKitDOMHTMLTableRowElement.set_ch(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_row_element_set_ch(@self, value);
end;

procedure TWebKitDOMHTMLTableRowElement.set_ch_off(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_row_element_set_ch_off(@self, value);
end;

procedure TWebKitDOMHTMLTableRowElement.set_v_align(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_table_row_element_set_v_align(@self, value);
end;

function TWebKitDOMHTMLTextAreaElement.check_validity: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_check_validity(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_access_key: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_access_key(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_autofocus: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_autofocus(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_cols: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_cols(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_default_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_default_value(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_disabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_disabled(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_form: PWebKitDOMHTMLFormElement; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_form(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_labels: PWebKitDOMNodeList; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_labels(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_max_length: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_max_length(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_name(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_placeholder: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_placeholder(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_read_only: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_read_only(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_required: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_required(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_rows: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_rows(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_selection_end: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_selection_end(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_selection_start: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_selection_start(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_text_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_text_length(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_validation_message: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_validation_message(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_validity: PWebKitDOMValidityState; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_validity(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_value: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_value(@self);
end;

function TWebKitDOMHTMLTextAreaElement.get_will_validate: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_text_area_element_get_will_validate(@self);
end;

procedure TWebKitDOMHTMLTextAreaElement.select; cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_select(@self);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_access_key(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_access_key(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_autofocus(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_autofocus(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_cols(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_cols(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_custom_validity(error: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_custom_validity(@self, error);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_default_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_default_value(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_disabled(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_disabled(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_max_length(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_max_length(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_name(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_name(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_placeholder(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_placeholder(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_read_only(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_read_only(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_required(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_required(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_rows(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_rows(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_selection_end(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_selection_end(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_selection_range(start: glong; end_: glong); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_selection_range(@self, start, end_);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_selection_start(value: glong); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_selection_start(@self, value);
end;

procedure TWebKitDOMHTMLTextAreaElement.set_value(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_text_area_element_set_value(@self, value);
end;

function TWebKitDOMHTMLTitleElement.get_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_title_element_get_text(@self);
end;

procedure TWebKitDOMHTMLTitleElement.set_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_title_element_set_text(@self, value);
end;

function TWebKitDOMHTMLUListElement.get_compact: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_htmlu_list_element_get_compact(@self);
end;

procedure TWebKitDOMHTMLUListElement.set_compact(value: gboolean); cdecl;
begin
  WebKit3.webkit_dom_htmlu_list_element_set_compact(@self, value);
end;

function TWebKitDOMHTMLVideoElement.get_height: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_video_element_get_height(@self);
end;

function TWebKitDOMHTMLVideoElement.get_poster: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_html_video_element_get_poster(@self);
end;

function TWebKitDOMHTMLVideoElement.get_video_height: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_video_element_get_video_height(@self);
end;

function TWebKitDOMHTMLVideoElement.get_video_width: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_video_element_get_video_width(@self);
end;

function TWebKitDOMHTMLVideoElement.get_webkit_displaying_fullscreen: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_video_element_get_webkit_displaying_fullscreen(@self);
end;

function TWebKitDOMHTMLVideoElement.get_webkit_supports_fullscreen: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_html_video_element_get_webkit_supports_fullscreen(@self);
end;

function TWebKitDOMHTMLVideoElement.get_width: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_html_video_element_get_width(@self);
end;

procedure TWebKitDOMHTMLVideoElement.set_height(value: gulong); cdecl;
begin
  WebKit3.webkit_dom_html_video_element_set_height(@self, value);
end;

procedure TWebKitDOMHTMLVideoElement.set_poster(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_html_video_element_set_poster(@self, value);
end;

procedure TWebKitDOMHTMLVideoElement.set_width(value: gulong); cdecl;
begin
  WebKit3.webkit_dom_html_video_element_set_width(@self, value);
end;

procedure TWebKitDOMHTMLVideoElement.webkit_enter_full_screen(isUserGesture: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_video_element_webkit_enter_full_screen(@self, isUserGesture);
end;

procedure TWebKitDOMHTMLVideoElement.webkit_enter_fullscreen(isUserGesture: gboolean); cdecl;
begin
  WebKit3.webkit_dom_html_video_element_webkit_enter_fullscreen(@self, isUserGesture);
end;

procedure TWebKitDOMHTMLVideoElement.webkit_exit_full_screen; cdecl;
begin
  WebKit3.webkit_dom_html_video_element_webkit_exit_full_screen(@self);
end;

procedure TWebKitDOMHTMLVideoElement.webkit_exit_fullscreen; cdecl;
begin
  WebKit3.webkit_dom_html_video_element_webkit_exit_fullscreen(@self);
end;

function TWebKitDOMLocation.get_origin: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_location_get_origin(@self);
end;

function TWebKitDOMLocation.get_parameter(name: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_location_get_parameter(@self, name);
end;

procedure TWebKitDOMMediaList.append_medium(new_medium: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_media_list_append_medium(@self, new_medium);
end;

procedure TWebKitDOMMediaList.delete_medium(old_medium: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_media_list_delete_medium(@self, old_medium);
end;

function TWebKitDOMMediaList.get_length: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_media_list_get_length(@self);
end;

function TWebKitDOMMediaList.get_media_text: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_media_list_get_media_text(@self);
end;

function TWebKitDOMMediaList.item(index: gulong): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_media_list_item(@self, index);
end;

procedure TWebKitDOMMediaList.set_media_text(value: Pgchar); cdecl;
begin
  WebKit3.webkit_dom_media_list_set_media_text(@self, value);
end;

function TWebKitDOMUIEvent.get_char_code: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_char_code(@self);
end;

function TWebKitDOMUIEvent.get_detail: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_detail(@self);
end;

function TWebKitDOMUIEvent.get_key_code: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_key_code(@self);
end;

function TWebKitDOMUIEvent.get_layer_x: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_layer_x(@self);
end;

function TWebKitDOMUIEvent.get_layer_y: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_layer_y(@self);
end;

function TWebKitDOMUIEvent.get_page_x: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_page_x(@self);
end;

function TWebKitDOMUIEvent.get_page_y: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_page_y(@self);
end;

function TWebKitDOMUIEvent.get_view: PWebKitDOMDOMWindow; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_view(@self);
end;

function TWebKitDOMUIEvent.get_which: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_ui_event_get_which(@self);
end;

procedure TWebKitDOMUIEvent.init_ui_event(type_: Pgchar; can_bubble: gboolean; cancelable: gboolean; view: PWebKitDOMDOMWindow; detail: glong); cdecl;
begin
  WebKit3.webkit_dom_ui_event_init_ui_event(@self, type_, can_bubble, cancelable, view, detail);
end;

function TWebKitDOMMouseEvent.get_alt_key: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_alt_key(@self);
end;

function TWebKitDOMMouseEvent.get_button: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_button(@self);
end;

function TWebKitDOMMouseEvent.get_client_x: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_client_x(@self);
end;

function TWebKitDOMMouseEvent.get_client_y: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_client_y(@self);
end;

function TWebKitDOMMouseEvent.get_ctrl_key: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_ctrl_key(@self);
end;

function TWebKitDOMMouseEvent.get_from_element: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_from_element(@self);
end;

function TWebKitDOMMouseEvent.get_meta_key: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_meta_key(@self);
end;

function TWebKitDOMMouseEvent.get_offset_x: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_offset_x(@self);
end;

function TWebKitDOMMouseEvent.get_offset_y: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_offset_y(@self);
end;

function TWebKitDOMMouseEvent.get_related_target: PWebKitDOMEventTarget; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_related_target(@self);
end;

function TWebKitDOMMouseEvent.get_screen_x: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_screen_x(@self);
end;

function TWebKitDOMMouseEvent.get_screen_y: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_screen_y(@self);
end;

function TWebKitDOMMouseEvent.get_shift_key: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_shift_key(@self);
end;

function TWebKitDOMMouseEvent.get_to_element: PWebKitDOMNode; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_to_element(@self);
end;

function TWebKitDOMMouseEvent.get_x: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_x(@self);
end;

function TWebKitDOMMouseEvent.get_y: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_mouse_event_get_y(@self);
end;

procedure TWebKitDOMMouseEvent.init_mouse_event(type_: Pgchar; can_bubble: gboolean; cancelable: gboolean; view: PWebKitDOMDOMWindow; detail: glong; screen_x: glong; screen_y: glong; client_x: glong; client_y: glong; ctrl_key: gboolean; alt_key: gboolean; shift_key: gboolean; meta_key: gboolean; button: gushort; related_target: PWebKitDOMEventTarget); cdecl;
begin
  WebKit3.webkit_dom_mouse_event_init_mouse_event(@self, type_, can_bubble, cancelable, view, detail, screen_x, screen_y, client_x, client_y, ctrl_key, alt_key, shift_key, meta_key, button, related_target);
end;

function TWebKitDOMNavigator.get_app_code_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_app_code_name(@self);
end;

function TWebKitDOMNavigator.get_app_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_app_name(@self);
end;

function TWebKitDOMNavigator.get_app_version: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_app_version(@self);
end;

function TWebKitDOMNavigator.get_cookie_enabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_cookie_enabled(@self);
end;

function TWebKitDOMNavigator.get_language: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_language(@self);
end;

function TWebKitDOMNavigator.get_mime_types: PWebKitDOMDOMMimeTypeArray; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_mime_types(@self);
end;

function TWebKitDOMNavigator.get_on_line: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_on_line(@self);
end;

function TWebKitDOMNavigator.get_platform: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_platform(@self);
end;

function TWebKitDOMNavigator.get_plugins: PWebKitDOMDOMPluginArray; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_plugins(@self);
end;

function TWebKitDOMNavigator.get_product: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_product(@self);
end;

function TWebKitDOMNavigator.get_product_sub: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_product_sub(@self);
end;

procedure TWebKitDOMNavigator.get_storage_updates; cdecl;
begin
  WebKit3.webkit_dom_navigator_get_storage_updates(@self);
end;

function TWebKitDOMNavigator.get_user_agent: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_user_agent(@self);
end;

function TWebKitDOMNavigator.get_vendor: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_vendor(@self);
end;

function TWebKitDOMNavigator.get_vendor_sub: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_get_vendor_sub(@self);
end;

function TWebKitDOMNavigator.java_enabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_navigator_java_enabled(@self);
end;

function TWebKitDOMScreen.get_avail_height: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_screen_get_avail_height(@self);
end;

function TWebKitDOMScreen.get_avail_left: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_screen_get_avail_left(@self);
end;

function TWebKitDOMScreen.get_avail_top: glong; cdecl;
begin
  Result := WebKit3.webkit_dom_screen_get_avail_top(@self);
end;

function TWebKitDOMScreen.get_avail_width: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_screen_get_avail_width(@self);
end;

function TWebKitDOMScreen.get_color_depth: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_screen_get_color_depth(@self);
end;

function TWebKitDOMScreen.get_height: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_screen_get_height(@self);
end;

function TWebKitDOMScreen.get_pixel_depth: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_screen_get_pixel_depth(@self);
end;

function TWebKitDOMScreen.get_width: gulong; cdecl;
begin
  Result := WebKit3.webkit_dom_screen_get_width(@self);
end;

function TWebKitDOMWebKitAnimation.get_delay: gdouble; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_get_delay(@self);
end;

function TWebKitDOMWebKitAnimation.get_direction: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_get_direction(@self);
end;

function TWebKitDOMWebKitAnimation.get_duration: gdouble; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_get_duration(@self);
end;

function TWebKitDOMWebKitAnimation.get_elapsed_time: gdouble; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_get_elapsed_time(@self);
end;

function TWebKitDOMWebKitAnimation.get_ended: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_get_ended(@self);
end;

function TWebKitDOMWebKitAnimation.get_fill_mode: gushort; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_get_fill_mode(@self);
end;

function TWebKitDOMWebKitAnimation.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_get_name(@self);
end;

function TWebKitDOMWebKitAnimation.get_paused: gboolean; cdecl;
begin
  Result := WebKit3.webkit_dom_webkit_animation_get_paused(@self);
end;

procedure TWebKitDOMWebKitAnimation.pause; cdecl;
begin
  WebKit3.webkit_dom_webkit_animation_pause(@self);
end;

procedure TWebKitDOMWebKitAnimation.play; cdecl;
begin
  WebKit3.webkit_dom_webkit_animation_play(@self);
end;

procedure TWebKitDOMWebKitAnimation.set_elapsed_time(value: gdouble); cdecl;
begin
  WebKit3.webkit_dom_webkit_animation_set_elapsed_time(@self, value);
end;

function TWebKitDownload.new(request: PWebKitNetworkRequest): PWebKitDownload; cdecl;
begin
  Result := WebKit3.webkit_download_new(request);
end;

procedure TWebKitDownload.cancel; cdecl;
begin
  WebKit3.webkit_download_cancel(@self);
end;

function TWebKitDownload.get_current_size: guint64; cdecl;
begin
  Result := WebKit3.webkit_download_get_current_size(@self);
end;

function TWebKitDownload.get_destination_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_download_get_destination_uri(@self);
end;

function TWebKitDownload.get_elapsed_time: gdouble; cdecl;
begin
  Result := WebKit3.webkit_download_get_elapsed_time(@self);
end;

function TWebKitDownload.get_network_request: PWebKitNetworkRequest; cdecl;
begin
  Result := WebKit3.webkit_download_get_network_request(@self);
end;

function TWebKitDownload.get_network_response: PWebKitNetworkResponse; cdecl;
begin
  Result := WebKit3.webkit_download_get_network_response(@self);
end;

function TWebKitDownload.get_progress: gdouble; cdecl;
begin
  Result := WebKit3.webkit_download_get_progress(@self);
end;

function TWebKitDownload.get_status: TWebKitDownloadStatus; cdecl;
begin
  Result := WebKit3.webkit_download_get_status(@self);
end;

function TWebKitDownload.get_suggested_filename: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_download_get_suggested_filename(@self);
end;

function TWebKitDownload.get_total_size: guint64; cdecl;
begin
  Result := WebKit3.webkit_download_get_total_size(@self);
end;

function TWebKitDownload.get_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_download_get_uri(@self);
end;

procedure TWebKitDownload.set_destination_uri(destination_uri: Pgchar); cdecl;
begin
  WebKit3.webkit_download_set_destination_uri(@self, destination_uri);
end;

procedure TWebKitDownload.start; cdecl;
begin
  WebKit3.webkit_download_start(@self);
end;

function TWebKitNetworkRequest.new(uri: Pgchar): PWebKitNetworkRequest; cdecl;
begin
  Result := WebKit3.webkit_network_request_new(uri);
end;

function TWebKitNetworkRequest.get_message: PSoupMessage; cdecl;
begin
  Result := WebKit3.webkit_network_request_get_message(@self);
end;

function TWebKitNetworkRequest.get_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_network_request_get_uri(@self);
end;

procedure TWebKitNetworkRequest.set_uri(uri: Pgchar); cdecl;
begin
  WebKit3.webkit_network_request_set_uri(@self, uri);
end;

function TWebKitNetworkResponse.new(uri: Pgchar): PWebKitNetworkResponse; cdecl;
begin
  Result := WebKit3.webkit_network_response_new(uri);
end;

function TWebKitNetworkResponse.get_message: PSoupMessage; cdecl;
begin
  Result := WebKit3.webkit_network_response_get_message(@self);
end;

function TWebKitNetworkResponse.get_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_network_response_get_uri(@self);
end;

procedure TWebKitNetworkResponse.set_uri(uri: Pgchar); cdecl;
begin
  WebKit3.webkit_network_response_set_uri(@self, uri);
end;


function TWebKitWebFrame.new(web_view: PWebKitWebView): PWebKitWebFrame; cdecl;
begin
  Result := WebKit3.webkit_web_frame_new(web_view);
end;

function TWebKitWebFrame.find_frame(name: Pgchar): PWebKitWebFrame; cdecl;
begin
  Result := WebKit3.webkit_web_frame_find_frame(@self, name);
end;

function TWebKitWebFrame.get_data_source: PWebKitWebDataSource; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_data_source(@self);
end;

function TWebKitWebFrame.get_global_context: TJSGlobalContextRef; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_global_context(@self);
end;

function TWebKitWebFrame.get_horizontal_scrollbar_policy: TGtkPolicyType; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_horizontal_scrollbar_policy(@self);
end;

function TWebKitWebFrame.get_load_status: TWebKitLoadStatus; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_load_status(@self);
end;

function TWebKitWebFrame.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_name(@self);
end;

function TWebKitWebFrame.get_network_response: PWebKitNetworkResponse; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_network_response(@self);
end;

function TWebKitWebFrame.get_parent: PWebKitWebFrame; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_parent(@self);
end;

function TWebKitWebFrame.get_provisional_data_source: PWebKitWebDataSource; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_provisional_data_source(@self);
end;

function TWebKitWebFrame.get_security_origin: PWebKitSecurityOrigin; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_security_origin(@self);
end;

function TWebKitWebFrame.get_title: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_title(@self);
end;

function TWebKitWebFrame.get_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_uri(@self);
end;

function TWebKitWebFrame.get_vertical_scrollbar_policy: TGtkPolicyType; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_vertical_scrollbar_policy(@self);
end;

function TWebKitWebFrame.get_web_view: PWebKitWebView; cdecl;
begin
  Result := WebKit3.webkit_web_frame_get_web_view(@self);
end;

procedure TWebKitWebFrame.load_alternate_string(content: Pgchar; base_url: Pgchar; unreachable_url: Pgchar); cdecl;
begin
  WebKit3.webkit_web_frame_load_alternate_string(@self, content, base_url, unreachable_url);
end;

procedure TWebKitWebFrame.load_request(request: PWebKitNetworkRequest); cdecl;
begin
  WebKit3.webkit_web_frame_load_request(@self, request);
end;

procedure TWebKitWebFrame.load_string(content: Pgchar; mime_type: Pgchar; encoding: Pgchar; base_uri: Pgchar); cdecl;
begin
  WebKit3.webkit_web_frame_load_string(@self, content, mime_type, encoding, base_uri);
end;

procedure TWebKitWebFrame.load_uri(uri: Pgchar); cdecl;
begin
  WebKit3.webkit_web_frame_load_uri(@self, uri);
end;

procedure TWebKitWebFrame.print; cdecl;
begin
  WebKit3.webkit_web_frame_print(@self);
end;

function TWebKitWebFrame.print_full(operation: PGtkPrintOperation; action: TGtkPrintOperationAction): TGtkPrintOperationResult; cdecl;
begin
  Result := WebKit3.webkit_web_frame_print_full(@self, operation, action);
end;

procedure TWebKitWebFrame.reload; cdecl;
begin
  WebKit3.webkit_web_frame_reload(@self);
end;

procedure TWebKitWebFrame.stop_loading; cdecl;
begin
  WebKit3.webkit_web_frame_stop_loading(@self);
end;

procedure TWebKitIconDatabase.clear; cdecl;
begin
  WebKit3.webkit_icon_database_clear(@self);
end;

function TWebKitIconDatabase.get_icon_pixbuf(page_uri: Pgchar): PGdkPixbuf; cdecl;
begin
  Result := WebKit3.webkit_icon_database_get_icon_pixbuf(@self, page_uri);
end;

function TWebKitIconDatabase.get_icon_uri(page_uri: Pgchar): Pgchar; cdecl;
begin
  Result := WebKit3.webkit_icon_database_get_icon_uri(@self, page_uri);
end;

function TWebKitIconDatabase.get_path: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_icon_database_get_path(@self);
end;

procedure TWebKitIconDatabase.set_path(path: Pgchar); cdecl;
begin
  WebKit3.webkit_icon_database_set_path(@self, path);
end;

function TWebKitSecurityOrigin.get_all_web_databases: PGList; cdecl;
begin
  Result := WebKit3.webkit_security_origin_get_all_web_databases(@self);
end;

function TWebKitSecurityOrigin.get_host: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_security_origin_get_host(@self);
end;

function TWebKitSecurityOrigin.get_port: guint; cdecl;
begin
  Result := WebKit3.webkit_security_origin_get_port(@self);
end;

function TWebKitSecurityOrigin.get_protocol: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_security_origin_get_protocol(@self);
end;

function TWebKitSecurityOrigin.get_web_database_quota: guint64; cdecl;
begin
  Result := WebKit3.webkit_security_origin_get_web_database_quota(@self);
end;

function TWebKitSecurityOrigin.get_web_database_usage: guint64; cdecl;
begin
  Result := WebKit3.webkit_security_origin_get_web_database_usage(@self);
end;

procedure TWebKitSecurityOrigin.set_web_database_quota(quota: guint64); cdecl;
begin
  WebKit3.webkit_security_origin_set_web_database_quota(@self, quota);
end;

procedure TWebKitViewportAttributes.recompute; cdecl;
begin
  WebKit3.webkit_viewport_attributes_recompute(@self);
end;

function TWebKitWebBackForwardList.new_with_web_view(web_view: PWebKitWebView): PWebKitWebBackForwardList; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_new_with_web_view(web_view);
end;

function TWebKitWebHistoryItem.new: PWebKitWebHistoryItem; cdecl;
begin
  Result := WebKit3.webkit_web_history_item_new();
end;

function TWebKitWebHistoryItem.new_with_data(uri: Pgchar; title: Pgchar): PWebKitWebHistoryItem; cdecl;
begin
  Result := WebKit3.webkit_web_history_item_new_with_data(uri, title);
end;

function TWebKitWebHistoryItem.copy: PWebKitWebHistoryItem; cdecl;
begin
  Result := WebKit3.webkit_web_history_item_copy(@self);
end;

function TWebKitWebHistoryItem.get_alternate_title: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_history_item_get_alternate_title(@self);
end;

function TWebKitWebHistoryItem.get_last_visited_time: gdouble; cdecl;
begin
  Result := WebKit3.webkit_web_history_item_get_last_visited_time(@self);
end;

function TWebKitWebHistoryItem.get_original_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_history_item_get_original_uri(@self);
end;

function TWebKitWebHistoryItem.get_title: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_history_item_get_title(@self);
end;

function TWebKitWebHistoryItem.get_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_history_item_get_uri(@self);
end;

procedure TWebKitWebHistoryItem.set_alternate_title(title: Pgchar); cdecl;
begin
  WebKit3.webkit_web_history_item_set_alternate_title(@self, title);
end;

procedure TWebKitWebBackForwardList.add_item(history_item: TWebKitWebHistoryItem); cdecl;
begin
  WebKit3.webkit_web_back_forward_list_add_item(@self, history_item);
end;

procedure TWebKitWebBackForwardList.clear; cdecl;
begin
  WebKit3.webkit_web_back_forward_list_clear(@self);
end;

function TWebKitWebBackForwardList.contains_item(history_item: TWebKitWebHistoryItem): gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_contains_item(@self, history_item);
end;

function TWebKitWebBackForwardList.get_back_item: TWebKitWebHistoryItem; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_back_item(@self);
end;

function TWebKitWebBackForwardList.get_back_length: gint; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_back_length(@self);
end;

function TWebKitWebBackForwardList.get_back_list_with_limit(limit: gint): PGList; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_back_list_with_limit(@self, limit);
end;

function TWebKitWebBackForwardList.get_current_item: TWebKitWebHistoryItem; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_current_item(@self);
end;

function TWebKitWebBackForwardList.get_forward_item: TWebKitWebHistoryItem; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_forward_item(@self);
end;

function TWebKitWebBackForwardList.get_forward_length: gint; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_forward_length(@self);
end;

function TWebKitWebBackForwardList.get_forward_list_with_limit(limit: gint): PGList; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_forward_list_with_limit(@self, limit);
end;

function TWebKitWebBackForwardList.get_limit: gint; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_limit(@self);
end;

function TWebKitWebBackForwardList.get_nth_item(index: gint): TWebKitWebHistoryItem; cdecl;
begin
  Result := WebKit3.webkit_web_back_forward_list_get_nth_item(@self, index);
end;

procedure TWebKitWebBackForwardList.go_back; cdecl;
begin
  WebKit3.webkit_web_back_forward_list_go_back(@self);
end;

procedure TWebKitWebBackForwardList.go_forward; cdecl;
begin
  WebKit3.webkit_web_back_forward_list_go_forward(@self);
end;

procedure TWebKitWebBackForwardList.go_to_item(history_item: TWebKitWebHistoryItem); cdecl;
begin
  WebKit3.webkit_web_back_forward_list_go_to_item(@self, history_item);
end;

procedure TWebKitWebBackForwardList.set_limit(limit: gint); cdecl;
begin
  WebKit3.webkit_web_back_forward_list_set_limit(@self, limit);
end;

function TWebKitWebView.new: PWebKitWebView; cdecl;
begin
  Result := WebKit3.webkit_web_view_new();
end;

function TWebKitWebView.can_copy_clipboard: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_copy_clipboard(@self);
end;

function TWebKitWebView.can_cut_clipboard: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_cut_clipboard(@self);
end;

function TWebKitWebView.can_go_back: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_go_back(@self);
end;

function TWebKitWebView.can_go_back_or_forward(steps: gint): gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_go_back_or_forward(@self, steps);
end;

function TWebKitWebView.can_go_forward: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_go_forward(@self);
end;

function TWebKitWebView.can_paste_clipboard: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_paste_clipboard(@self);
end;

function TWebKitWebView.can_redo: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_redo(@self);
end;

function TWebKitWebView.can_show_mime_type(mime_type: Pgchar): gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_show_mime_type(@self, mime_type);
end;

function TWebKitWebView.can_undo: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_can_undo(@self);
end;

procedure TWebKitWebView.copy_clipboard; cdecl;
begin
  WebKit3.webkit_web_view_copy_clipboard(@self);
end;

procedure TWebKitWebView.cut_clipboard; cdecl;
begin
  WebKit3.webkit_web_view_cut_clipboard(@self);
end;

procedure TWebKitWebView.delete_selection; cdecl;
begin
  WebKit3.webkit_web_view_delete_selection(@self);
end;

procedure TWebKitWebView.execute_script(script: Pgchar); cdecl;
begin
  WebKit3.webkit_web_view_execute_script(@self, script);
end;

function TWebKitWebView.get_back_forward_list: PWebKitWebBackForwardList; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_back_forward_list(@self);
end;

function TWebKitWebView.get_copy_target_list: PGtkTargetList; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_copy_target_list(@self);
end;

function TWebKitWebView.get_custom_encoding: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_custom_encoding(@self);
end;

function TWebKitWebView.get_dom_document: PWebKitDOMDocument; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_dom_document(@self);
end;

function TWebKitWebView.get_editable: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_editable(@self);
end;

function TWebKitWebView.get_encoding: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_encoding(@self);
end;

function TWebKitWebView.get_focused_frame: PWebKitWebFrame; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_focused_frame(@self);
end;

function TWebKitWebView.get_full_content_zoom: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_full_content_zoom(@self);
end;

function TWebKitWebView.get_hit_test_result(event: PGdkEventButton): PWebKitHitTestResult; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_hit_test_result(@self, event);
end;

function TWebKitWebView.get_icon_pixbuf: PGdkPixbuf; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_icon_pixbuf(@self);
end;

function TWebKitWebView.get_icon_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_icon_uri(@self);
end;

function TWebKitWebView.get_inspector: PWebKitWebInspector; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_inspector(@self);
end;

function TWebKitWebView.get_load_status: TWebKitLoadStatus; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_load_status(@self);
end;

function TWebKitWebView.get_main_frame: PWebKitWebFrame; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_main_frame(@self);
end;

function TWebKitWebView.get_paste_target_list: PGtkTargetList; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_paste_target_list(@self);
end;

function TWebKitWebView.get_progress: gdouble; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_progress(@self);
end;

function TWebKitWebView.get_settings: PWebKitWebSettings; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_settings(@self);
end;

function TWebKitWebView.get_title: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_title(@self);
end;

function TWebKitWebView.get_transparent: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_transparent(@self);
end;

function TWebKitWebView.get_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_uri(@self);
end;

function TWebKitWebView.get_view_mode: TWebKitWebViewViewMode; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_view_mode(@self);
end;

function TWebKitWebView.get_view_source_mode: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_view_source_mode(@self);
end;

function TWebKitWebView.get_viewport_attributes: PWebKitViewportAttributes; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_viewport_attributes(@self);
end;

function TWebKitWebView.get_window_features: PWebKitWebWindowFeatures; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_window_features(@self);
end;

function TWebKitWebView.get_zoom_level: gfloat; cdecl;
begin
  Result := WebKit3.webkit_web_view_get_zoom_level(@self);
end;

procedure TWebKitWebView.go_back; cdecl;
begin
  WebKit3.webkit_web_view_go_back(@self);
end;

procedure TWebKitWebView.go_back_or_forward(steps: gint); cdecl;
begin
  WebKit3.webkit_web_view_go_back_or_forward(@self, steps);
end;

procedure TWebKitWebView.go_forward; cdecl;
begin
  WebKit3.webkit_web_view_go_forward(@self);
end;

function TWebKitWebView.go_to_back_forward_item(item: PWebKitWebHistoryItem): gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_go_to_back_forward_item(@self, item);
end;

function TWebKitWebView.has_selection: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_has_selection(@self);
end;

procedure TWebKitWebView.load_html_string(content: Pgchar; base_uri: Pgchar); cdecl;
begin
  WebKit3.webkit_web_view_load_html_string(@self, content, base_uri);
end;

procedure TWebKitWebView.load_request(request: PWebKitNetworkRequest); cdecl;
begin
  WebKit3.webkit_web_view_load_request(@self, request);
end;

procedure TWebKitWebView.load_string(content: Pgchar; mime_type: Pgchar; encoding: Pgchar; base_uri: Pgchar); cdecl;
begin
  WebKit3.webkit_web_view_load_string(@self, content, mime_type, encoding, base_uri);
end;

procedure TWebKitWebView.load_uri(uri: Pgchar); cdecl;
begin
  WebKit3.webkit_web_view_load_uri(@self, uri);
end;

function TWebKitWebView.mark_text_matches(string_: Pgchar; case_sensitive: gboolean; limit: guint): guint; cdecl;
begin
  Result := WebKit3.webkit_web_view_mark_text_matches(@self, string_, case_sensitive, limit);
end;

procedure TWebKitWebView.move_cursor(step: TGtkMovementStep; count: gint); cdecl;
begin
  WebKit3.webkit_web_view_move_cursor(@self, step, count);
end;

procedure TWebKitWebView.open(uri: Pgchar); cdecl;
begin
  WebKit3.webkit_web_view_open(@self, uri);
end;

procedure TWebKitWebView.paste_clipboard; cdecl;
begin
  WebKit3.webkit_web_view_paste_clipboard(@self);
end;

procedure TWebKitWebView.redo; cdecl;
begin
  WebKit3.webkit_web_view_redo(@self);
end;

procedure TWebKitWebView.reload; cdecl;
begin
  WebKit3.webkit_web_view_reload(@self);
end;

procedure TWebKitWebView.reload_bypass_cache; cdecl;
begin
  WebKit3.webkit_web_view_reload_bypass_cache(@self);
end;

function TWebKitWebView.search_text(text: Pgchar; case_sensitive: gboolean; forward: gboolean; wrap: gboolean): gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_view_search_text(@self, text, case_sensitive, forward, wrap);
end;

procedure TWebKitWebView.select_all; cdecl;
begin
  WebKit3.webkit_web_view_select_all(@self);
end;

procedure TWebKitWebView.set_custom_encoding(encoding: Pgchar); cdecl;
begin
  WebKit3.webkit_web_view_set_custom_encoding(@self, encoding);
end;

procedure TWebKitWebView.set_editable(flag: gboolean); cdecl;
begin
  WebKit3.webkit_web_view_set_editable(@self, flag);
end;

procedure TWebKitWebView.set_full_content_zoom(full_content_zoom: gboolean); cdecl;
begin
  WebKit3.webkit_web_view_set_full_content_zoom(@self, full_content_zoom);
end;

procedure TWebKitWebView.set_highlight_text_matches(highlight: gboolean); cdecl;
begin
  WebKit3.webkit_web_view_set_highlight_text_matches(@self, highlight);
end;

procedure TWebKitWebView.set_maintains_back_forward_list(flag: gboolean); cdecl;
begin
  WebKit3.webkit_web_view_set_maintains_back_forward_list(@self, flag);
end;

procedure TWebKitWebView.set_settings(settings: PWebKitWebSettings); cdecl;
begin
  WebKit3.webkit_web_view_set_settings(@self, settings);
end;

procedure TWebKitWebView.set_transparent(flag: gboolean); cdecl;
begin
  WebKit3.webkit_web_view_set_transparent(@self, flag);
end;

procedure TWebKitWebView.set_view_mode(mode: TWebKitWebViewViewMode); cdecl;
begin
  WebKit3.webkit_web_view_set_view_mode(@self, mode);
end;

procedure TWebKitWebView.set_view_source_mode(view_source_mode: gboolean); cdecl;
begin
  WebKit3.webkit_web_view_set_view_source_mode(@self, view_source_mode);
end;

procedure TWebKitWebView.set_zoom_level(zoom_level: gfloat); cdecl;
begin
  WebKit3.webkit_web_view_set_zoom_level(@self, zoom_level);
end;

procedure TWebKitWebView.stop_loading; cdecl;
begin
  WebKit3.webkit_web_view_stop_loading(@self);
end;

procedure TWebKitWebView.undo; cdecl;
begin
  WebKit3.webkit_web_view_undo(@self);
end;

procedure TWebKitWebView.unmark_text_matches; cdecl;
begin
  WebKit3.webkit_web_view_unmark_text_matches(@self);
end;

procedure TWebKitWebView.zoom_in; cdecl;
begin
  WebKit3.webkit_web_view_zoom_in(@self);
end;

procedure TWebKitWebView.zoom_out; cdecl;
begin
  WebKit3.webkit_web_view_zoom_out(@self);
end;

function TWebKitWebDataSource.new: PWebKitWebDataSource; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_new();
end;

function TWebKitWebDataSource.new_with_request(request: PWebKitNetworkRequest): PWebKitWebDataSource; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_new_with_request(request);
end;

function TWebKitWebDataSource.get_data: PGString; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_get_data(@self);
end;

function TWebKitWebDataSource.get_encoding: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_get_encoding(@self);
end;

function TWebKitWebDataSource.get_initial_request: PWebKitNetworkRequest; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_get_initial_request(@self);
end;

function TWebKitWebDataSource.get_main_resource: PWebKitWebResource; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_get_main_resource(@self);
end;

function TWebKitWebDataSource.get_request: PWebKitNetworkRequest; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_get_request(@self);
end;

function TWebKitWebDataSource.get_subresources: PGList; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_get_subresources(@self);
end;

function TWebKitWebDataSource.get_unreachable_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_get_unreachable_uri(@self);
end;

function TWebKitWebDataSource.get_web_frame: PWebKitWebFrame; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_get_web_frame(@self);
end;

function TWebKitWebDataSource.is_loading: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_data_source_is_loading(@self);
end;

function TWebKitWebResource.new(data: Pgchar; size: gssize; uri: Pgchar; mime_type: Pgchar; encoding: Pgchar; frame_name: Pgchar): PWebKitWebResource; cdecl;
begin
  Result := WebKit3.webkit_web_resource_new(data, size, uri, mime_type, encoding, frame_name);
end;

function TWebKitWebResource.get_data: PGString; cdecl;
begin
  Result := WebKit3.webkit_web_resource_get_data(@self);
end;

function TWebKitWebResource.get_encoding: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_resource_get_encoding(@self);
end;

function TWebKitWebResource.get_frame_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_resource_get_frame_name(@self);
end;

function TWebKitWebResource.get_mime_type: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_resource_get_mime_type(@self);
end;

function TWebKitWebResource.get_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_resource_get_uri(@self);
end;

function TWebKitWebDatabase.get_display_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_database_get_display_name(@self);
end;

function TWebKitWebDatabase.get_expected_size: guint64; cdecl;
begin
  Result := WebKit3.webkit_web_database_get_expected_size(@self);
end;

function TWebKitWebDatabase.get_filename: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_database_get_filename(@self);
end;

function TWebKitWebDatabase.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_database_get_name(@self);
end;

function TWebKitWebDatabase.get_security_origin: PWebKitSecurityOrigin; cdecl;
begin
  Result := WebKit3.webkit_web_database_get_security_origin(@self);
end;

function TWebKitWebDatabase.get_size: guint64; cdecl;
begin
  Result := WebKit3.webkit_web_database_get_size(@self);
end;

procedure TWebKitWebDatabase.remove; cdecl;
begin
  WebKit3.webkit_web_database_remove(@self);
end;

procedure TWebKitWebInspector.close; cdecl;
begin
  WebKit3.webkit_web_inspector_close(@self);
end;

function TWebKitWebInspector.get_inspected_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_inspector_get_inspected_uri(@self);
end;

function TWebKitWebInspector.get_web_view: PWebKitWebView; cdecl;
begin
  Result := WebKit3.webkit_web_inspector_get_web_view(@self);
end;

procedure TWebKitWebInspector.inspect_coordinates(x: gdouble; y: gdouble); cdecl;
begin
  WebKit3.webkit_web_inspector_inspect_coordinates(@self, x, y);
end;

procedure TWebKitWebInspector.inspect_node(node: PWebKitDOMNode); cdecl;
begin
  WebKit3.webkit_web_inspector_inspect_node(@self, node);
end;

procedure TWebKitWebInspector.show; cdecl;
begin
  WebKit3.webkit_web_inspector_show(@self);
end;

function TWebKitWebNavigationAction.get_button: gint; cdecl;
begin
  Result := WebKit3.webkit_web_navigation_action_get_button(@self);
end;

function TWebKitWebNavigationAction.get_modifier_state: gint; cdecl;
begin
  Result := WebKit3.webkit_web_navigation_action_get_modifier_state(@self);
end;

function TWebKitWebNavigationAction.get_original_uri: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_navigation_action_get_original_uri(@self);
end;

function TWebKitWebNavigationAction.get_reason: TWebKitWebNavigationReason; cdecl;
begin
  Result := WebKit3.webkit_web_navigation_action_get_reason(@self);
end;

function TWebKitWebNavigationAction.get_target_frame: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_navigation_action_get_target_frame(@self);
end;

procedure TWebKitWebNavigationAction.set_original_uri(originalUri: Pgchar); cdecl;
begin
  WebKit3.webkit_web_navigation_action_set_original_uri(@self, originalUri);
end;

procedure TWebKitWebNavigationAction.set_reason(reason: TWebKitWebNavigationReason); cdecl;
begin
  WebKit3.webkit_web_navigation_action_set_reason(@self, reason);
end;

function TWebKitWebPlugin.get_description: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_plugin_get_description(@self);
end;

function TWebKitWebPlugin.get_enabled: gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_plugin_get_enabled(@self);
end;

function TWebKitWebPlugin.get_mimetypes: PGSList; cdecl;
begin
  Result := WebKit3.webkit_web_plugin_get_mimetypes(@self);
end;

function TWebKitWebPlugin.get_name: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_plugin_get_name(@self);
end;

function TWebKitWebPlugin.get_path: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_plugin_get_path(@self);
end;

procedure TWebKitWebPlugin.set_enabled(param0: gboolean); cdecl;
begin
  WebKit3.webkit_web_plugin_set_enabled(@self, param0);
end;

procedure TWebKitWebPluginDatabase.plugins_list_free(param0: PGSList); cdecl;
begin
  WebKit3.webkit_web_plugin_database_plugins_list_free(param0);
end;

function TWebKitWebPluginDatabase.get_plugin_for_mimetype(param0: Pgchar): PWebKitWebPlugin; cdecl;
begin
  Result := WebKit3.webkit_web_plugin_database_get_plugin_for_mimetype(@self, param0);
end;

function TWebKitWebPluginDatabase.get_plugins: PGSList; cdecl;
begin
  Result := WebKit3.webkit_web_plugin_database_get_plugins(@self);
end;

procedure TWebKitWebPluginDatabase.refresh; cdecl;
begin
  WebKit3.webkit_web_plugin_database_refresh(@self);
end;

procedure TWebKitWebPolicyDecision.download; cdecl;
begin
  WebKit3.webkit_web_policy_decision_download(@self);
end;

procedure TWebKitWebPolicyDecision.ignore; cdecl;
begin
  WebKit3.webkit_web_policy_decision_ignore(@self);
end;

procedure TWebKitWebPolicyDecision.use; cdecl;
begin
  WebKit3.webkit_web_policy_decision_use(@self);
end;

function TWebKitWebSettings.new: PWebKitWebSettings; cdecl;
begin
  Result := WebKit3.webkit_web_settings_new();
end;

function TWebKitWebSettings.copy: PWebKitWebSettings; cdecl;
begin
  Result := WebKit3.webkit_web_settings_copy(@self);
end;

function TWebKitWebSettings.get_user_agent: Pgchar; cdecl;
begin
  Result := WebKit3.webkit_web_settings_get_user_agent(@self);
end;

function TWebKitWebWindowFeatures.new: PWebKitWebWindowFeatures; cdecl;
begin
  Result := WebKit3.webkit_web_window_features_new();
end;

function TWebKitWebWindowFeatures.equal(features2: PWebKitWebWindowFeatures): gboolean; cdecl;
begin
  Result := WebKit3.webkit_web_window_features_equal(@self, features2);
end;

end.