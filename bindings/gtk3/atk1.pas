unit Atk1;

{$MODE OBJFPC}{$H+}

{$PACKRECORDS C}
{$BITPACKING ON}
{$MODESWITCH DUPLICATELOCALS+}

{$LINKLIB libatk-1.0.so.0}
interface
uses
  CTypes, GLib2, GObject2;

const
  Atk1_library = 'libatk-1.0.so.0';


type
  TAtkCoordType = Integer;
const
  { AtkCoordType }
  ATK_XY_SCREEN: TAtkCoordType = 0;
  ATK_XY_WINDOW: TAtkCoordType = 1;

type
  TAtkLayer = Integer;
const
  { AtkLayer }
  ATK_LAYER_INVALID: TAtkLayer = 0;
  ATK_LAYER_BACKGROUND: TAtkLayer = 1;
  ATK_LAYER_CANVAS: TAtkLayer = 2;
  ATK_LAYER_WIDGET: TAtkLayer = 3;
  ATK_LAYER_MDI: TAtkLayer = 4;
  ATK_LAYER_POPUP: TAtkLayer = 5;
  ATK_LAYER_OVERLAY: TAtkLayer = 6;
  ATK_LAYER_WINDOW: TAtkLayer = 7;

  { AtkHyperlinkStateFlags }
  ATK_HYPERLINK_IS_INLINE_ = 1;

type
  TAtkKeyEventType = Integer;
const
  { AtkKeyEventType }
  ATK_KEY_EVENT_PRESS: TAtkKeyEventType = 0;
  ATK_KEY_EVENT_RELEASE: TAtkKeyEventType = 1;
  ATK_KEY_EVENT_LAST_DEFINED: TAtkKeyEventType = 2;

type
  TAtkTextClipType = Integer;
const
  { AtkTextClipType }
  ATK_TEXT_CLIP_NONE: TAtkTextClipType = 0;
  ATK_TEXT_CLIP_MIN: TAtkTextClipType = 1;
  ATK_TEXT_CLIP_MAX: TAtkTextClipType = 2;
  ATK_TEXT_CLIP_BOTH: TAtkTextClipType = 3;

type
  TAtkTextBoundary = Integer;
const
  { AtkTextBoundary }
  ATK_TEXT_BOUNDARY_CHAR: TAtkTextBoundary = 0;
  ATK_TEXT_BOUNDARY_WORD_START: TAtkTextBoundary = 1;
  ATK_TEXT_BOUNDARY_WORD_END: TAtkTextBoundary = 2;
  ATK_TEXT_BOUNDARY_SENTENCE_START: TAtkTextBoundary = 3;
  ATK_TEXT_BOUNDARY_SENTENCE_END: TAtkTextBoundary = 4;
  ATK_TEXT_BOUNDARY_LINE_START: TAtkTextBoundary = 5;
  ATK_TEXT_BOUNDARY_LINE_END: TAtkTextBoundary = 6;

type
  TAtkRole = Integer;
const
  { AtkRole }
  ATK_ROLE_INVALID: TAtkRole = 0;
  ATK_ROLE_ACCEL_LABEL: TAtkRole = 1;
  ATK_ROLE_ALERT: TAtkRole = 2;
  ATK_ROLE_ANIMATION: TAtkRole = 3;
  ATK_ROLE_ARROW: TAtkRole = 4;
  ATK_ROLE_CALENDAR: TAtkRole = 5;
  ATK_ROLE_CANVAS: TAtkRole = 6;
  ATK_ROLE_CHECK_BOX: TAtkRole = 7;
  ATK_ROLE_CHECK_MENU_ITEM: TAtkRole = 8;
  ATK_ROLE_COLOR_CHOOSER: TAtkRole = 9;
  ATK_ROLE_COLUMN_HEADER: TAtkRole = 10;
  ATK_ROLE_COMBO_BOX: TAtkRole = 11;
  ATK_ROLE_DATE_EDITOR: TAtkRole = 12;
  ATK_ROLE_DESKTOP_ICON: TAtkRole = 13;
  ATK_ROLE_DESKTOP_FRAME: TAtkRole = 14;
  ATK_ROLE_DIAL: TAtkRole = 15;
  ATK_ROLE_DIALOG: TAtkRole = 16;
  ATK_ROLE_DIRECTORY_PANE: TAtkRole = 17;
  ATK_ROLE_DRAWING_AREA: TAtkRole = 18;
  ATK_ROLE_FILE_CHOOSER: TAtkRole = 19;
  ATK_ROLE_FILLER: TAtkRole = 20;
  ATK_ROLE_FONT_CHOOSER: TAtkRole = 21;
  ATK_ROLE_FRAME: TAtkRole = 22;
  ATK_ROLE_GLASS_PANE: TAtkRole = 23;
  ATK_ROLE_HTML_CONTAINER: TAtkRole = 24;
  ATK_ROLE_ICON: TAtkRole = 25;
  ATK_ROLE_IMAGE: TAtkRole = 26;
  ATK_ROLE_INTERNAL_FRAME: TAtkRole = 27;
  ATK_ROLE_LABEL: TAtkRole = 28;
  ATK_ROLE_LAYERED_PANE: TAtkRole = 29;
  ATK_ROLE_LIST: TAtkRole = 30;
  ATK_ROLE_LIST_ITEM: TAtkRole = 31;
  ATK_ROLE_MENU: TAtkRole = 32;
  ATK_ROLE_MENU_BAR: TAtkRole = 33;
  ATK_ROLE_MENU_ITEM: TAtkRole = 34;
  ATK_ROLE_OPTION_PANE: TAtkRole = 35;
  ATK_ROLE_PAGE_TAB: TAtkRole = 36;
  ATK_ROLE_PAGE_TAB_LIST: TAtkRole = 37;
  ATK_ROLE_PANEL: TAtkRole = 38;
  ATK_ROLE_PASSWORD_TEXT: TAtkRole = 39;
  ATK_ROLE_POPUP_MENU: TAtkRole = 40;
  ATK_ROLE_PROGRESS_BAR: TAtkRole = 41;
  ATK_ROLE_PUSH_BUTTON: TAtkRole = 42;
  ATK_ROLE_RADIO_BUTTON: TAtkRole = 43;
  ATK_ROLE_RADIO_MENU_ITEM: TAtkRole = 44;
  ATK_ROLE_ROOT_PANE: TAtkRole = 45;
  ATK_ROLE_ROW_HEADER: TAtkRole = 46;
  ATK_ROLE_SCROLL_BAR: TAtkRole = 47;
  ATK_ROLE_SCROLL_PANE: TAtkRole = 48;
  ATK_ROLE_SEPARATOR: TAtkRole = 49;
  ATK_ROLE_SLIDER: TAtkRole = 50;
  ATK_ROLE_SPLIT_PANE: TAtkRole = 51;
  ATK_ROLE_SPIN_BUTTON: TAtkRole = 52;
  ATK_ROLE_STATUSBAR: TAtkRole = 53;
  ATK_ROLE_TABLE: TAtkRole = 54;
  ATK_ROLE_TABLE_CELL: TAtkRole = 55;
  ATK_ROLE_TABLE_COLUMN_HEADER: TAtkRole = 56;
  ATK_ROLE_TABLE_ROW_HEADER: TAtkRole = 57;
  ATK_ROLE_TEAR_OFF_MENU_ITEM: TAtkRole = 58;
  ATK_ROLE_TERMINAL: TAtkRole = 59;
  ATK_ROLE_TEXT: TAtkRole = 60;
  ATK_ROLE_TOGGLE_BUTTON: TAtkRole = 61;
  ATK_ROLE_TOOL_BAR: TAtkRole = 62;
  ATK_ROLE_TOOL_TIP: TAtkRole = 63;
  ATK_ROLE_TREE: TAtkRole = 64;
  ATK_ROLE_TREE_TABLE: TAtkRole = 65;
  ATK_ROLE_UNKNOWN: TAtkRole = 66;
  ATK_ROLE_VIEWPORT: TAtkRole = 67;
  ATK_ROLE_WINDOW: TAtkRole = 68;
  ATK_ROLE_HEADER: TAtkRole = 69;
  ATK_ROLE_FOOTER: TAtkRole = 70;
  ATK_ROLE_PARAGRAPH: TAtkRole = 71;
  ATK_ROLE_RULER: TAtkRole = 72;
  ATK_ROLE_APPLICATION: TAtkRole = 73;
  ATK_ROLE_AUTOCOMPLETE: TAtkRole = 74;
  ATK_ROLE_EDITBAR: TAtkRole = 75;
  ATK_ROLE_EMBEDDED: TAtkRole = 76;
  ATK_ROLE_ENTRY: TAtkRole = 77;
  ATK_ROLE_CHART: TAtkRole = 78;
  ATK_ROLE_CAPTION: TAtkRole = 79;
  ATK_ROLE_DOCUMENT_FRAME: TAtkRole = 80;
  ATK_ROLE_HEADING: TAtkRole = 81;
  ATK_ROLE_PAGE: TAtkRole = 82;
  ATK_ROLE_SECTION: TAtkRole = 83;
  ATK_ROLE_REDUNDANT_OBJECT: TAtkRole = 84;
  ATK_ROLE_FORM: TAtkRole = 85;
  ATK_ROLE_LINK: TAtkRole = 86;
  ATK_ROLE_INPUT_METHOD_WINDOW: TAtkRole = 87;
  ATK_ROLE_TABLE_ROW: TAtkRole = 88;
  ATK_ROLE_TREE_ITEM: TAtkRole = 89;
  ATK_ROLE_DOCUMENT_SPREADSHEET: TAtkRole = 90;
  ATK_ROLE_DOCUMENT_PRESENTATION: TAtkRole = 91;
  ATK_ROLE_DOCUMENT_TEXT: TAtkRole = 92;
  ATK_ROLE_DOCUMENT_WEB: TAtkRole = 93;
  ATK_ROLE_DOCUMENT_EMAIL: TAtkRole = 94;
  ATK_ROLE_COMMENT: TAtkRole = 95;
  ATK_ROLE_LIST_BOX: TAtkRole = 96;
  ATK_ROLE_GROUPING: TAtkRole = 97;
  ATK_ROLE_IMAGE_MAP: TAtkRole = 98;
  ATK_ROLE_NOTIFICATION: TAtkRole = 99;
  ATK_ROLE_INFO_BAR: TAtkRole = 100;
  ATK_ROLE_LAST_DEFINED: TAtkRole = 101;

type
  TAtkRelationType = Integer;
const
  { AtkRelationType }
  ATK_RELATION_NULL: TAtkRelationType = 0;
  ATK_RELATION_CONTROLLED_BY: TAtkRelationType = 1;
  ATK_RELATION_CONTROLLER_FOR: TAtkRelationType = 2;
  ATK_RELATION_LABEL_FOR: TAtkRelationType = 3;
  ATK_RELATION_LABELLED_BY: TAtkRelationType = 4;
  ATK_RELATION_MEMBER_OF: TAtkRelationType = 5;
  ATK_RELATION_NODE_CHILD_OF: TAtkRelationType = 6;
  ATK_RELATION_FLOWS_TO: TAtkRelationType = 7;
  ATK_RELATION_FLOWS_FROM: TAtkRelationType = 8;
  ATK_RELATION_SUBWINDOW_OF: TAtkRelationType = 9;
  ATK_RELATION_EMBEDS: TAtkRelationType = 10;
  ATK_RELATION_EMBEDDED_BY: TAtkRelationType = 11;
  ATK_RELATION_POPUP_FOR: TAtkRelationType = 12;
  ATK_RELATION_PARENT_WINDOW_OF: TAtkRelationType = 13;
  ATK_RELATION_DESCRIBED_BY: TAtkRelationType = 14;
  ATK_RELATION_DESCRIPTION_FOR: TAtkRelationType = 15;
  ATK_RELATION_NODE_PARENT_OF: TAtkRelationType = 16;
  ATK_RELATION_LAST_DEFINED: TAtkRelationType = 17;

type
  TAtkStateType = Integer;
const
  { AtkStateType }
  ATK_STATE_INVALID: TAtkStateType = 0;
  ATK_STATE_ACTIVE: TAtkStateType = 1;
  ATK_STATE_ARMED: TAtkStateType = 2;
  ATK_STATE_BUSY: TAtkStateType = 3;
  ATK_STATE_CHECKED: TAtkStateType = 4;
  ATK_STATE_DEFUNCT: TAtkStateType = 5;
  ATK_STATE_EDITABLE: TAtkStateType = 6;
  ATK_STATE_ENABLED: TAtkStateType = 7;
  ATK_STATE_EXPANDABLE: TAtkStateType = 8;
  ATK_STATE_EXPANDED: TAtkStateType = 9;
  ATK_STATE_FOCUSABLE: TAtkStateType = 10;
  ATK_STATE_FOCUSED: TAtkStateType = 11;
  ATK_STATE_HORIZONTAL: TAtkStateType = 12;
  ATK_STATE_ICONIFIED: TAtkStateType = 13;
  ATK_STATE_MODAL: TAtkStateType = 14;
  ATK_STATE_MULTI_LINE: TAtkStateType = 15;
  ATK_STATE_MULTISELECTABLE: TAtkStateType = 16;
  ATK_STATE_OPAQUE: TAtkStateType = 17;
  ATK_STATE_PRESSED: TAtkStateType = 18;
  ATK_STATE_RESIZABLE: TAtkStateType = 19;
  ATK_STATE_SELECTABLE: TAtkStateType = 20;
  ATK_STATE_SELECTED: TAtkStateType = 21;
  ATK_STATE_SENSITIVE: TAtkStateType = 22;
  ATK_STATE_SHOWING: TAtkStateType = 23;
  ATK_STATE_SINGLE_LINE: TAtkStateType = 24;
  ATK_STATE_STALE: TAtkStateType = 25;
  ATK_STATE_TRANSIENT: TAtkStateType = 26;
  ATK_STATE_VERTICAL: TAtkStateType = 27;
  ATK_STATE_VISIBLE: TAtkStateType = 28;
  ATK_STATE_MANAGES_DESCENDANTS: TAtkStateType = 29;
  ATK_STATE_INDETERMINATE: TAtkStateType = 30;
  ATK_STATE_TRUNCATED: TAtkStateType = 31;
  ATK_STATE_REQUIRED: TAtkStateType = 32;
  ATK_STATE_INVALID_ENTRY: TAtkStateType = 33;
  ATK_STATE_SUPPORTS_AUTOCOMPLETION: TAtkStateType = 34;
  ATK_STATE_SELECTABLE_TEXT: TAtkStateType = 35;
  ATK_STATE_DEFAULT: TAtkStateType = 36;
  ATK_STATE_ANIMATED: TAtkStateType = 37;
  ATK_STATE_VISITED: TAtkStateType = 38;
  ATK_STATE_LAST_DEFINED: TAtkStateType = 39;

type
  TAtkTextAttribute = Integer;
const
  { AtkTextAttribute }
  ATK_TEXT_ATTR_INVALID: TAtkTextAttribute = 0;
  ATK_TEXT_ATTR_LEFT_MARGIN: TAtkTextAttribute = 1;
  ATK_TEXT_ATTR_RIGHT_MARGIN: TAtkTextAttribute = 2;
  ATK_TEXT_ATTR_INDENT: TAtkTextAttribute = 3;
  ATK_TEXT_ATTR_INVISIBLE: TAtkTextAttribute = 4;
  ATK_TEXT_ATTR_EDITABLE: TAtkTextAttribute = 5;
  ATK_TEXT_ATTR_PIXELS_ABOVE_LINES: TAtkTextAttribute = 6;
  ATK_TEXT_ATTR_PIXELS_BELOW_LINES: TAtkTextAttribute = 7;
  ATK_TEXT_ATTR_PIXELS_INSIDE_WRAP: TAtkTextAttribute = 8;
  ATK_TEXT_ATTR_BG_FULL_HEIGHT: TAtkTextAttribute = 9;
  ATK_TEXT_ATTR_RISE: TAtkTextAttribute = 10;
  ATK_TEXT_ATTR_UNDERLINE: TAtkTextAttribute = 11;
  ATK_TEXT_ATTR_STRIKETHROUGH: TAtkTextAttribute = 12;
  ATK_TEXT_ATTR_SIZE: TAtkTextAttribute = 13;
  ATK_TEXT_ATTR_SCALE: TAtkTextAttribute = 14;
  ATK_TEXT_ATTR_WEIGHT: TAtkTextAttribute = 15;
  ATK_TEXT_ATTR_LANGUAGE: TAtkTextAttribute = 16;
  ATK_TEXT_ATTR_FAMILY_NAME: TAtkTextAttribute = 17;
  ATK_TEXT_ATTR_BG_COLOR: TAtkTextAttribute = 18;
  ATK_TEXT_ATTR_FG_COLOR: TAtkTextAttribute = 19;
  ATK_TEXT_ATTR_BG_STIPPLE: TAtkTextAttribute = 20;
  ATK_TEXT_ATTR_FG_STIPPLE: TAtkTextAttribute = 21;
  ATK_TEXT_ATTR_WRAP_MODE: TAtkTextAttribute = 22;
  ATK_TEXT_ATTR_DIRECTION: TAtkTextAttribute = 23;
  ATK_TEXT_ATTR_JUSTIFICATION: TAtkTextAttribute = 24;
  ATK_TEXT_ATTR_STRETCH: TAtkTextAttribute = 25;
  ATK_TEXT_ATTR_VARIANT: TAtkTextAttribute = 26;
  ATK_TEXT_ATTR_STYLE: TAtkTextAttribute = 27;
  ATK_TEXT_ATTR_LAST_DEFINED: TAtkTextAttribute = 28;
type

  PPAtkAttributeSet = ^PAtkAttributeSet;
  PAtkAttributeSet = ^TAtkAttributeSet;
  TAtkAttributeSet = TGSList;

  PPAtkState = ^PAtkState;
  PAtkState = ^TAtkState;
  TAtkState = guint64;

  PPAtkAction = ^PAtkAction;
  PAtkAction = ^TAtkAction;
  TAtkAction = object
    function do_action(i: gint): gboolean; cdecl; inline;
    function get_description(i: gint): Pgchar; cdecl; inline;
    function get_keybinding(i: gint): Pgchar; cdecl; inline;
    function get_localized_name(i: gint): Pgchar; cdecl; inline;
    function get_n_actions: gint; cdecl; inline;
    function get_name(i: gint): Pgchar; cdecl; inline;
    function set_description(i: gint; desc: Pgchar): gboolean; cdecl; inline;
  end;
  TAtkFunction = function(data: gpointer): gboolean; cdecl;

  PPAtkActionIface = ^PAtkActionIface;
  PAtkActionIface = ^TAtkActionIface;

  PPAtkFunction = ^PAtkFunction;
  PAtkFunction = ^TAtkFunction;
  TAtkActionIface = object
    parent: TGTypeInterface;
    do_action: function(action: PAtkAction; i: gint): gboolean; cdecl;
    get_n_actions: function(action: PAtkAction): gint; cdecl;
    get_description: function(action: PAtkAction; i: gint): Pgchar; cdecl;
    get_name: function(action: PAtkAction; i: gint): Pgchar; cdecl;
    get_keybinding: function(action: PAtkAction; i: gint): Pgchar; cdecl;
    set_description: function(action: PAtkAction; i: gint; desc: Pgchar): gboolean; cdecl;
    get_localized_name: function(action: PAtkAction; i: gint): Pgchar; cdecl;
    pad2: TAtkFunction;
  end;

  PPAtkAttribute = ^PAtkAttribute;
  PAtkAttribute = ^TAtkAttribute;

  TAtkAttribute = record
    name: Pgchar;
    value: Pgchar;
  end;


  TAtkFocusHandler = procedure(param0: PGObject; param1: gboolean); cdecl;

  PPAtkRectangle = ^PAtkRectangle;
  PAtkRectangle = ^TAtkRectangle;
  TAtkRectangle = object
    x: gint;
    y: gint;
    width: gint;
    height: gint;
  end;

  PPAtkCoordType = ^PAtkCoordType;
  PAtkCoordType = ^TAtkCoordType;

  PPAtkLayer = ^PAtkLayer;
  PAtkLayer = ^TAtkLayer;

  PPAtkComponent = ^PAtkComponent;
  PAtkComponent = ^TAtkComponent;

  PPAtkFocusHandler = ^PAtkFocusHandler;
  PAtkFocusHandler = ^TAtkFocusHandler;
  TAtkComponent = object
    bounds_changed: procedure(object_: TAtkRectangle); cdecl;
    function add_focus_handler(handler: TAtkFocusHandler): guint; cdecl; inline;
    function contains(x: gint; y: gint; coord_type: TAtkCoordType): gboolean; cdecl; inline;
    function get_alpha: gdouble; cdecl; inline;
    procedure get_extents(x: Pgint; y: Pgint; width: Pgint; height: Pgint; coord_type: TAtkCoordType); cdecl; inline;
    function get_layer: TAtkLayer; cdecl; inline;
    function get_mdi_zorder: gint; cdecl; inline;
    procedure get_position(x: Pgint; y: Pgint; coord_type: TAtkCoordType); cdecl; inline;
    procedure get_size(width: Pgint; height: Pgint); cdecl; inline;
    function grab_focus: gboolean; cdecl; inline;
    function ref_accessible_at_point(x: gint; y: gint; coord_type: TAtkCoordType): PGObject; cdecl; inline;
    procedure remove_focus_handler(handler_id: guint); cdecl; inline;
    function set_extents(x: gint; y: gint; width: gint; height: gint; coord_type: TAtkCoordType): gboolean; cdecl; inline;
    function set_position(x: gint; y: gint; coord_type: TAtkCoordType): gboolean; cdecl; inline;
    function set_size(width: gint; height: gint): gboolean; cdecl; inline;
  end;

  PPAtkComponentIface = ^PAtkComponentIface;
  PAtkComponentIface = ^TAtkComponentIface;
  TAtkComponentIface = object
    parent: TGTypeInterface;
    add_focus_handler: function(component: PAtkComponent; handler: TAtkFocusHandler): guint; cdecl;
    contains: function(component: PAtkComponent; x: gint; y: gint; coord_type: TAtkCoordType): gboolean; cdecl;
    ref_accessible_at_point: function(component: PAtkComponent; x: gint; y: gint; coord_type: TAtkCoordType): PGObject; cdecl;
    get_extents: procedure(component: PAtkComponent; x: Pgint; y: Pgint; width: Pgint; height: Pgint; coord_type: TAtkCoordType); cdecl;
    get_position: procedure(component: PAtkComponent; x: Pgint; y: Pgint; coord_type: TAtkCoordType); cdecl;
    get_size: procedure(component: PAtkComponent; width: Pgint; height: Pgint); cdecl;
    grab_focus: function(component: PAtkComponent): gboolean; cdecl;
    remove_focus_handler: procedure(component: PAtkComponent; handler_id: guint); cdecl;
    set_extents: function(component: PAtkComponent; x: gint; y: gint; width: gint; height: gint; coord_type: TAtkCoordType): gboolean; cdecl;
    set_position: function(component: PAtkComponent; x: gint; y: gint; coord_type: TAtkCoordType): gboolean; cdecl;
    set_size: function(component: PAtkComponent; width: gint; height: gint): gboolean; cdecl;
    get_layer: function(component: PAtkComponent): TAtkLayer; cdecl;
    get_mdi_zorder: function(component: PAtkComponent): gint; cdecl;
    bounds_changed: procedure(component: PAtkComponent; bounds: PAtkRectangle); cdecl;
    get_alpha: function(component: PAtkComponent): gdouble; cdecl;
  end;

  PPAtkDocument = ^PAtkDocument;
  PAtkDocument = ^TAtkDocument;
  TAtkDocument = object
    load_complete: procedure; cdecl;
    load_stopped: procedure; cdecl;
    reload: procedure; cdecl;
    function get_attribute_value(attribute_name: Pgchar): Pgchar; cdecl; inline;
    function get_attributes: PAtkAttributeSet; cdecl; inline;
    function get_document: gpointer; cdecl; inline;
    function get_document_type: Pgchar; cdecl; inline;
    function get_locale: Pgchar; cdecl; inline;
    function set_attribute_value(attribute_name: Pgchar; attribute_value: Pgchar): gboolean; cdecl; inline;
  end;

  PPAtkDocumentIface = ^PAtkDocumentIface;
  PAtkDocumentIface = ^TAtkDocumentIface;
  TAtkDocumentIface = object
    parent: TGTypeInterface;
    get_document_type: function(document: PAtkDocument): Pgchar; cdecl;
    get_document: function(document: PAtkDocument): gpointer; cdecl;
    get_document_locale: function(document: PAtkDocument): Pgchar; cdecl;
    get_document_attributes: function(document: PAtkDocument): PAtkAttributeSet; cdecl;
    get_document_attribute_value: function(document: PAtkDocument; attribute_name: Pgchar): Pgchar; cdecl;
    set_document_attribute: function(document: PAtkDocument; attribute_name: Pgchar; attribute_value: Pgchar): gboolean; cdecl;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
    pad3: TAtkFunction;
    pad4: TAtkFunction;
  end;

  PPAtkEditableText = ^PAtkEditableText;
  PAtkEditableText = ^TAtkEditableText;
  TAtkEditableText = object
    procedure copy_text(start_pos: gint; end_pos: gint); cdecl; inline;
    procedure cut_text(start_pos: gint; end_pos: gint); cdecl; inline;
    procedure delete_text(start_pos: gint; end_pos: gint); cdecl; inline;
    procedure insert_text(string_: Pgchar; length: gint; position: Pgint); cdecl; inline;
    procedure paste_text(position: gint); cdecl; inline;
    function set_run_attributes(attrib_set: PAtkAttributeSet; start_offset: gint; end_offset: gint): gboolean; cdecl; inline;
    procedure set_text_contents(string_: Pgchar); cdecl; inline;
  end;

  PPAtkEditableTextIface = ^PAtkEditableTextIface;
  PAtkEditableTextIface = ^TAtkEditableTextIface;
  TAtkEditableTextIface = object
    parent_interface: TGTypeInterface;
    set_run_attributes: function(text: PAtkEditableText; attrib_set: PAtkAttributeSet; start_offset: gint; end_offset: gint): gboolean; cdecl;
    set_text_contents: procedure(text: PAtkEditableText; string_: Pgchar); cdecl;
    insert_text: procedure(text: PAtkEditableText; string_: Pgchar; length: gint; position: Pgint); cdecl;
    copy_text: procedure(text: PAtkEditableText; start_pos: gint; end_pos: gint); cdecl;
    cut_text: procedure(text: PAtkEditableText; start_pos: gint; end_pos: gint); cdecl;
    delete_text: procedure(text: PAtkEditableText; start_pos: gint; end_pos: gint); cdecl;
    paste_text: procedure(text: PAtkEditableText; position: gint); cdecl;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
  end;
  TAtkEventListener = procedure(obj: PGObject); cdecl;
  TAtkEventListenerInit = procedure; cdecl;

  PPAtkGObjectAccessible = ^PAtkGObjectAccessible;
  PAtkGObjectAccessible = ^TAtkGObjectAccessible;
  TAtkGObjectAccessible = object
    parent: TGObject;
    function for_object(obj: PGObject): PGObject; cdecl; inline; static;
    function get_object: PGObject; cdecl; inline;
  end;

  PPAtkGObjectAccessibleClass = ^PAtkGObjectAccessibleClass;
  PAtkGObjectAccessibleClass = ^TAtkGObjectAccessibleClass;
  TAtkGObjectAccessibleClass = object
    parent_class: TGObjectClass;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
  end;

  PPAtkHyperlink = ^PAtkHyperlink;
  PAtkHyperlink = ^TAtkHyperlink;
  TAtkHyperlink = object(TGObject)
    function get_end_index: gint; cdecl; inline;
    function get_n_anchors: gint; cdecl; inline;
    function get_object(i: gint): PGObject; cdecl; inline;
    function get_start_index: gint; cdecl; inline;
    function get_uri(i: gint): Pgchar; cdecl; inline;
    function is_inline: gboolean; cdecl; inline;
    function is_selected_link: gboolean; cdecl; inline;
    function is_valid: gboolean; cdecl; inline;
    property end_index:  gint read get_end_index ;
    //property number_of_anchors: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_number_of_anchors ;
    //property selected_link: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_selected_link ;
    property start_index:  gint read get_start_index ;
  end;

  PPAtkHyperlinkClass = ^PAtkHyperlinkClass;
  PAtkHyperlinkClass = ^TAtkHyperlinkClass;
  TAtkHyperlinkClass = object
    parent: TGObjectClass;
    get_uri: function(link_: PAtkHyperlink; i: gint): Pgchar; cdecl;
    get_object: function(link_: PAtkHyperlink; i: gint): PGObject; cdecl;
    get_end_index: function(link_: PAtkHyperlink): gint; cdecl;
    get_start_index: function(link_: PAtkHyperlink): gint; cdecl;
    is_valid: function(link_: PAtkHyperlink): gboolean; cdecl;
    get_n_anchors: function(link_: PAtkHyperlink): gint; cdecl;
    link_state: function(link_: PAtkHyperlink): guint; cdecl;
    is_selected_link: function(link_: PAtkHyperlink): gboolean; cdecl;
    link_activated: procedure(link_: PAtkHyperlink); cdecl;
    pad1: TAtkFunction;
  end;

  PPAtkHyperlinkImpl = ^PAtkHyperlinkImpl;
  PAtkHyperlinkImpl = ^TAtkHyperlinkImpl;
  TAtkHyperlinkImpl = object
    function get_hyperlink: PAtkHyperlink; cdecl; inline;
  end;

  PPAtkHyperlinkImplIface = ^PAtkHyperlinkImplIface;
  PAtkHyperlinkImplIface = ^TAtkHyperlinkImplIface;
  TAtkHyperlinkImplIface = object
    parent: TGTypeInterface;
    get_hyperlink: function(impl: PAtkHyperlinkImpl): PAtkHyperlink; cdecl;
    pad1: TAtkFunction;
  end;
  TAtkHyperlinkStateFlags = packed object(TBitObject32)
  public
    property inline_: DWord index 1 read GetBit write SetBit;
  end;


  PPAtkHypertext = ^PAtkHypertext;
  PAtkHypertext = ^TAtkHypertext;
  TAtkHypertext = object
    link_selected: procedure(object_: gint); cdecl;
    function get_link(link_index: gint): PAtkHyperlink; cdecl; inline;
    function get_link_index(char_index: gint): gint; cdecl; inline;
    function get_n_links: gint; cdecl; inline;
  end;

  PPAtkHypertextIface = ^PAtkHypertextIface;
  PAtkHypertextIface = ^TAtkHypertextIface;
  TAtkHypertextIface = object
    parent: TGTypeInterface;
    get_link: function(hypertext: PAtkHypertext; link_index: gint): PAtkHyperlink; cdecl;
    get_n_links: function(hypertext: PAtkHypertext): gint; cdecl;
    get_link_index: function(hypertext: PAtkHypertext; char_index: gint): gint; cdecl;
    link_selected: procedure(hypertext: PAtkHypertext; link_index: gint); cdecl;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
    pad3: TAtkFunction;
  end;

  PPAtkImage = ^PAtkImage;
  PAtkImage = ^TAtkImage;
  TAtkImage = object
    function get_image_description: Pgchar; cdecl; inline;
    function get_image_locale: Pgchar; cdecl; inline;
    procedure get_image_position(x: Pgint; y: Pgint; coord_type: TAtkCoordType); cdecl; inline;
    procedure get_image_size(width: Pgint; height: Pgint); cdecl; inline;
    function set_image_description(description: Pgchar): gboolean; cdecl; inline;
  end;

  PPAtkImageIface = ^PAtkImageIface;
  PAtkImageIface = ^TAtkImageIface;
  TAtkImageIface = object
    parent: TGTypeInterface;
    get_image_position: procedure(image: PAtkImage; x: Pgint; y: Pgint; coord_type: TAtkCoordType); cdecl;
    get_image_description: function(image: PAtkImage): Pgchar; cdecl;
    get_image_size: procedure(image: PAtkImage; width: Pgint; height: Pgint); cdecl;
    set_image_description: function(image: PAtkImage; description: Pgchar): gboolean; cdecl;
    get_image_locale: function(image: PAtkImage): Pgchar; cdecl;
    pad1: TAtkFunction;
  end;

  PPAtkImplementor = ^PAtkImplementor;
  PAtkImplementor = ^TAtkImplementor;
  TAtkImplementor = object
    function ref_accessible: PGObject; cdecl; inline;
  end;

  PPAtkImplementorIface = ^PAtkImplementorIface;
  PAtkImplementorIface = ^TAtkImplementorIface;
  TAtkImplementorIface = object
  end;

  PPAtkKeyEventStruct = ^PAtkKeyEventStruct;
  PAtkKeyEventStruct = ^TAtkKeyEventStruct;

  TAtkKeyEventStruct = record
    type_: gint;
    state: guint;
    keyval: guint;
    length: gint;
    string_: Pgchar;
    keycode: guint16;
    timestamp: guint32;
  end;



  PPAtkKeyEventType = ^PAtkKeyEventType;
  PAtkKeyEventType = ^TAtkKeyEventType;
  TAtkKeySnoopFunc = function(event: PAtkKeyEventStruct; func_data: gpointer): gint; cdecl;

  PPAtkMisc = ^PAtkMisc;
  PAtkMisc = ^TAtkMisc;
  TAtkMisc = object(TGObject)
    function get_instance: PAtkMisc; cdecl; inline; static;
    procedure threads_enter; cdecl; inline;
    procedure threads_leave; cdecl; inline;
  end;

  PPAtkMiscClass = ^PAtkMiscClass;
  PAtkMiscClass = ^TAtkMiscClass;
  TAtkMiscClass = object
    parent: TGObjectClass;
    threads_enter: procedure(misc: PAtkMisc); cdecl;
    threads_leave: procedure(misc: PAtkMisc); cdecl;
    vfuncs: array [0..31] of gpointer;
  end;

  PPAtkSelection = ^PAtkSelection;
  PAtkSelection = ^TAtkSelection;

  PPAtkObject = ^PAtkObject;
  PAtkObject = ^TAtkObject;
  TAtkSelection = object
    selection_changed: procedure; cdecl;
    function add_selection(i: gint): gboolean; cdecl; inline;
    function clear_selection: gboolean; cdecl; inline;
    function get_selection_count: gint; cdecl; inline;
    function is_child_selected(i: gint): gboolean; cdecl; inline;
    function ref_selection(i: gint): PAtkObject; cdecl; inline;
    function remove_selection(i: gint): gboolean; cdecl; inline;
    function select_all_selection: gboolean; cdecl; inline;
  end;

  PPAtkTable = ^PAtkTable;
  PAtkTable = ^TAtkTable;
  TAtkTable = object
    column_deleted: procedure(object_: gint; p0: gint); cdecl;
    column_inserted: procedure(object_: gint; p0: gint); cdecl;
    column_reordered: procedure; cdecl;
    model_changed: procedure; cdecl;
    row_deleted: procedure(object_: gint; p0: gint); cdecl;
    row_inserted: procedure(object_: gint; p0: gint); cdecl;
    row_reordered: procedure; cdecl;
    function add_column_selection(column: gint): gboolean; cdecl; inline;
    function add_row_selection(row: gint): gboolean; cdecl; inline;
    function get_caption: PAtkObject; cdecl; inline;
    function get_column_at_index(index_: gint): gint; cdecl; inline;
    function get_column_description(column: gint): Pgchar; cdecl; inline;
    function get_column_extent_at(row: gint; column: gint): gint; cdecl; inline;
    function get_column_header(column: gint): PAtkObject; cdecl; inline;
    function get_index_at(row: gint; column: gint): gint; cdecl; inline;
    function get_n_columns: gint; cdecl; inline;
    function get_n_rows: gint; cdecl; inline;
    function get_row_at_index(index_: gint): gint; cdecl; inline;
    function get_row_description(row: gint): Pgchar; cdecl; inline;
    function get_row_extent_at(row: gint; column: gint): gint; cdecl; inline;
    function get_row_header(row: gint): PAtkObject; cdecl; inline;
    function get_selected_columns(selected: PPgint): gint; cdecl; inline;
    function get_selected_rows(selected: PPgint): gint; cdecl; inline;
    function get_summary: PAtkObject; cdecl; inline;
    function is_column_selected(column: gint): gboolean; cdecl; inline;
    function is_row_selected(row: gint): gboolean; cdecl; inline;
    function is_selected(row: gint; column: gint): gboolean; cdecl; inline;
    function ref_at(row: gint; column: gint): PAtkObject; cdecl; inline;
    function remove_column_selection(column: gint): gboolean; cdecl; inline;
    function remove_row_selection(row: gint): gboolean; cdecl; inline;
    procedure set_caption(caption: PAtkObject); cdecl; inline;
    procedure set_column_description(column: gint; description: Pgchar); cdecl; inline;
    procedure set_column_header(column: gint; header: PAtkObject); cdecl; inline;
    procedure set_row_description(row: gint; description: Pgchar); cdecl; inline;
    procedure set_row_header(row: gint; header: PAtkObject); cdecl; inline;
    procedure set_summary(accessible: PAtkObject); cdecl; inline;
  end;

  PPAtkText = ^PAtkText;
  PAtkText = ^TAtkText;

  PPAtkTextRange = ^PAtkTextRange;
  PAtkTextRange = ^TAtkTextRange;

  PPAtkTextRectangle = ^PAtkTextRectangle;
  PAtkTextRectangle = ^TAtkTextRectangle;

  PPAtkTextClipType = ^PAtkTextClipType;
  PAtkTextClipType = ^TAtkTextClipType;

  PPAtkTextBoundary = ^PAtkTextBoundary;
  PAtkTextBoundary = ^TAtkTextBoundary;
  TAtkText = object
    text_attributes_changed: procedure; cdecl;
    text_caret_moved: procedure(object_: gint); cdecl;
    text_changed: procedure(object_: gint; p0: gint); cdecl;
    text_insert: procedure(object_: gint; p0: gint; p1: gchar); cdecl;
    text_remove: procedure(object_: gint; p0: gint; p1: gchar); cdecl;
    text_selection_changed: procedure; cdecl;
    text_update: procedure(object_: gint; p0: gint; p1: gint; p2: gchar); cdecl;
    function add_selection(start_offset: gint; end_offset: gint): gboolean; cdecl; inline;
    function get_bounded_ranges(rect: PAtkTextRectangle; coord_type: TAtkCoordType; x_clip_type: TAtkTextClipType; y_clip_type: TAtkTextClipType): PPAtkTextRange; cdecl; inline;
    function get_caret_offset: gint; cdecl; inline;
    function get_character_at_offset(offset: gint): gunichar; cdecl; inline;
    function get_character_count: gint; cdecl; inline;
    procedure get_character_extents(offset: gint; x: Pgint; y: Pgint; width: Pgint; height: Pgint; coords: TAtkCoordType); cdecl; inline;
    function get_default_attributes: PAtkAttributeSet; cdecl; inline;
    function get_n_selections: gint; cdecl; inline;
    function get_offset_at_point(x: gint; y: gint; coords: TAtkCoordType): gint; cdecl; inline;
    procedure get_range_extents(start_offset: gint; end_offset: gint; coord_type: TAtkCoordType; rect: PAtkTextRectangle); cdecl; inline;
    function get_run_attributes(offset: gint; start_offset: Pgint; end_offset: Pgint): PAtkAttributeSet; cdecl; inline;
    function get_selection(selection_num: gint; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl; inline;
    function get_text(start_offset: gint; end_offset: gint): Pgchar; cdecl; inline;
    function get_text_after_offset(offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl; inline;
    function get_text_at_offset(offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl; inline;
    function get_text_before_offset(offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl; inline;
    function remove_selection(selection_num: gint): gboolean; cdecl; inline;
    function set_caret_offset(offset: gint): gboolean; cdecl; inline;
    function set_selection(selection_num: gint; start_offset: gint; end_offset: gint): gboolean; cdecl; inline;
  end;

  PPAtkWindow = ^PAtkWindow;
  PAtkWindow = ^TAtkWindow;
  TAtkWindow = object
    activate: procedure; cdecl;
    create: procedure; cdecl;
    deactivate: procedure; cdecl;
    destroy_: procedure; cdecl;
    maximize: procedure; cdecl;
    minimize: procedure; cdecl;
    move: procedure; cdecl;
    resize: procedure; cdecl;
    restore: procedure; cdecl;
  end;

  PPAtkNoOpObject = ^PAtkNoOpObject;
  PAtkNoOpObject = ^TAtkNoOpObject;
  TAtkNoOpObject = object
    parent: TGObject;
    function new(obj: PGObject): PAtkNoOpObject; cdecl; inline; static;
  end;

  PPAtkNoOpObjectClass = ^PAtkNoOpObjectClass;
  PAtkNoOpObjectClass = ^TAtkNoOpObjectClass;
  TAtkNoOpObjectClass = object
    parent_class: TGObjectClass;
  end;

  PPAtkObjectFactory = ^PAtkObjectFactory;
  PAtkObjectFactory = ^TAtkObjectFactory;
  TAtkObjectFactory = object(TGObject)
    function create_accessible(obj: PGObject): PAtkObject; cdecl; inline;
    function get_accessible_type: TGType; cdecl; inline;
    procedure invalidate; cdecl; inline;
  end;

  PPAtkNoOpObjectFactory = ^PAtkNoOpObjectFactory;
  PAtkNoOpObjectFactory = ^TAtkNoOpObjectFactory;
  TAtkNoOpObjectFactory = object(TAtkObjectFactory)
    function new: PAtkNoOpObjectFactory; cdecl; inline; static;
  end;

  PPAtkObjectFactoryClass = ^PAtkObjectFactoryClass;
  PAtkObjectFactoryClass = ^TAtkObjectFactoryClass;
  TAtkObjectFactoryClass = object
    parent_class: TGObjectClass;
    create_accessible: function(obj: PGObject): PAtkObject; cdecl;
    invalidate: procedure(factory: PAtkObjectFactory); cdecl;
    get_accessible_type: function: TGType; cdecl;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
  end;

  PPAtkNoOpObjectFactoryClass = ^PAtkNoOpObjectFactoryClass;
  PAtkNoOpObjectFactoryClass = ^TAtkNoOpObjectFactoryClass;
  TAtkNoOpObjectFactoryClass = object
    parent_class: TAtkObjectFactoryClass;
  end;

  PAtkPropertyValues = ^TAtkPropertyValues;
  TAtkPropertyChangeHandler = procedure(param0: PAtkObject; param1: PAtkPropertyValues); cdecl;

  PPAtkRole = ^PAtkRole;
  PAtkRole = ^TAtkRole;

  { AtkPropertyValues* }
  TAtkPropertyValues = record
    { opaque type }
    Unknown: Pointer;
  end;


  PPAtkRelationSet = ^PAtkRelationSet;
  PAtkRelationSet = ^TAtkRelationSet;

  PPAtkRelation = ^PAtkRelation;
  PAtkRelation = ^TAtkRelation;

  PPAtkRelationType = ^PAtkRelationType;
  PAtkRelationType = ^TAtkRelationType;
  TAtkRelationSet = object(TGObject)
    relations: gpointer;
    function new: PAtkRelationSet; cdecl; inline; static;
    procedure add(relation: PAtkRelation); cdecl; inline;
    procedure add_relation_by_type(relationship: TAtkRelationType; target: PAtkObject); cdecl; inline;
    function contains(relationship: TAtkRelationType): gboolean; cdecl; inline;
    function get_n_relations: gint; cdecl; inline;
    function get_relation(i: gint): PAtkRelation; cdecl; inline;
    function get_relation_by_type(relationship: TAtkRelationType): PAtkRelation; cdecl; inline;
    procedure remove(relation: PAtkRelation); cdecl; inline;
  end;

  PPAtkStateSet = ^PAtkStateSet;
  PAtkStateSet = ^TAtkStateSet;

  PPAtkStateType = ^PAtkStateType;
  PAtkStateType = ^TAtkStateType;
  TAtkStateSet = object(TGObject)
    function new: PAtkStateSet; cdecl; inline; static;
    function add_state(type_: TAtkStateType): gboolean; cdecl; inline;
    procedure add_states(types: PAtkStateType; n_types: gint); cdecl; inline;
    function and_sets(compare_set: PAtkStateSet): PAtkStateSet; cdecl; inline;
    procedure clear_states; cdecl; inline;
    function contains_state(type_: TAtkStateType): gboolean; cdecl; inline;
    function contains_states(types: PAtkStateType; n_types: gint): gboolean; cdecl; inline;
    function is_empty: gboolean; cdecl; inline;
    function or_sets(compare_set: PAtkStateSet): PAtkStateSet; cdecl; inline;
    function remove_state(type_: TAtkStateType): gboolean; cdecl; inline;
    function xor_sets(compare_set: PAtkStateSet): PAtkStateSet; cdecl; inline;
  end;

  PPAtkPropertyChangeHandler = ^PAtkPropertyChangeHandler;
  PAtkPropertyChangeHandler = ^TAtkPropertyChangeHandler;
  TAtkObject = object(TGObject)
    description: Pgchar;
    name: Pgchar;
    role: TAtkRole;
    relation_set: PAtkRelationSet;
    layer: TAtkLayer;
    function add_relationship(relationship: TAtkRelationType; target: PGObject): gboolean; cdecl; inline;
    function connect_property_change_handler(handler: PAtkPropertyChangeHandler): guint; cdecl; inline;
    function get_attributes: PAtkAttributeSet; cdecl; inline;
    function get_description: Pgchar; cdecl; inline;
    function get_index_in_parent: gint; cdecl; inline;
    function get_layer: TAtkLayer; cdecl; inline;
    function get_mdi_zorder: gint; cdecl; inline;
    function get_n_accessible_children: gint; cdecl; inline;
    function get_name: Pgchar; cdecl; inline;
    function get_parent: PGObject; cdecl; inline;
    function get_role: TAtkRole; cdecl; inline;
    procedure initialize(data: gpointer); cdecl; inline;
    procedure notify_state_change(state: TAtkState; value: gboolean); cdecl; inline;
    function ref_accessible_child(i: gint): PGObject; cdecl; inline;
    function ref_relation_set: PAtkRelationSet; cdecl; inline;
    function ref_state_set: PAtkStateSet; cdecl; inline;
    procedure remove_property_change_handler(handler_id: guint); cdecl; inline;
    function remove_relationship(relationship: TAtkRelationType; target: PGObject): gboolean; cdecl; inline;
    procedure set_description(description: Pgchar); cdecl; inline;
    procedure set_name(name: Pgchar); cdecl; inline;
    procedure set_parent(parent: PGObject); cdecl; inline;
    procedure set_role(role: TAtkRole); cdecl; inline;
    //property accessible_component_layer: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_component_layer ;
    //property accessible_component_mdi_zorder: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_component_mdi_zorder ;
    //property accessible_description: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_description  { property is writeable but setter not declared } ;
    //property accessible_hypertext_nlinks: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_hypertext_nlinks ;
    //property accessible_name: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_name  { property is writeable but setter not declared } ;
    //property accessible_parent: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_parent  { property is writeable but setter not declared } ;
    //property accessible_role: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_role  { property is writeable but setter not declared } ;
    //property accessible_table_caption: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_table_caption  { property is writeable but setter not declared } ;
    //property accessible_table_caption_object: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_table_caption_object  { property is writeable but setter not declared } ;
    //property accessible_table_column_description: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_table_column_description  { property is writeable but setter not declared } ;
    //property accessible_table_column_header: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_table_column_header  { property is writeable but setter not declared } ;
    //property accessible_table_row_description: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_table_row_description  { property is writeable but setter not declared } ;
    //property accessible_table_row_header: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_table_row_header  { property is writeable but setter not declared } ;
    //property accessible_table_summary: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_table_summary  { property is writeable but setter not declared } ;
    //property accessible_value: UNABLE_TO_FIND_TYPE_FOR_PROPERTY read get_accessible_value  { property is writeable but setter not declared } ;
  end;

  PPAtkObjectClass = ^PAtkObjectClass;
  PAtkObjectClass = ^TAtkObjectClass;
  TAtkObjectClass = object
    parent: TGObjectClass;
    get_name: function(accessible: PAtkObject): Pgchar; cdecl;
    get_description: function(accessible: PAtkObject): Pgchar; cdecl;
    get_parent: function(accessible: PAtkObject): PAtkObject; cdecl;
    get_n_children: function(accessible: PAtkObject): gint; cdecl;
    ref_child: function(accessible: PAtkObject; i: gint): PAtkObject; cdecl;
    get_index_in_parent: function(accessible: PAtkObject): gint; cdecl;
    ref_relation_set: function(accessible: PAtkObject): PAtkRelationSet; cdecl;
    get_role: function(accessible: PAtkObject): TAtkRole; cdecl;
    get_layer: function(accessible: PAtkObject): TAtkLayer; cdecl;
    get_mdi_zorder: function(accessible: PAtkObject): gint; cdecl;
    ref_state_set: function(accessible: PAtkObject): PAtkStateSet; cdecl;
    set_name: procedure(accessible: PAtkObject; name: Pgchar); cdecl;
    set_description: procedure(accessible: PAtkObject; description: Pgchar); cdecl;
    set_parent: procedure(accessible: PAtkObject; parent: PAtkObject); cdecl;
    set_role: procedure(accessible: PAtkObject; role: TAtkRole); cdecl;
    connect_property_change_handler: function(accessible: PAtkObject; handler: PAtkPropertyChangeHandler): guint; cdecl;
    remove_property_change_handler: procedure(accessible: PAtkObject; handler_id: guint); cdecl;
    initialize: procedure(accessible: PAtkObject; data: gpointer); cdecl;
    children_changed: procedure(accessible: PAtkObject; change_index: guint; changed_child: gpointer); cdecl;
    focus_event: procedure(accessible: PAtkObject; focus_in: gboolean); cdecl;
    property_change: procedure(accessible: PAtkObject; values: PAtkPropertyValues); cdecl;
    state_change: procedure(accessible: PAtkObject; name: Pgchar; state_set: gboolean); cdecl;
    visible_data_changed: procedure(accessible: PAtkObject); cdecl;
    active_descendant_changed: procedure(accessible: PAtkObject; child: Pgpointer); cdecl;
    get_attributes: function(accessible: PAtkObject): PAtkAttributeSet; cdecl;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
  end;

  PPAtkPlug = ^PAtkPlug;
  PAtkPlug = ^TAtkPlug;
  TAtkPlug = object(TAtkObject)
    function new: PAtkPlug; cdecl; inline; static;
    function get_id: Pgchar; cdecl; inline;
  end;

  PPAtkPlugClass = ^PAtkPlugClass;
  PAtkPlugClass = ^TAtkPlugClass;
  TAtkPlugClass = object
    parent_class: TAtkObjectClass;
    get_object_id: function(obj: PAtkPlug): Pgchar; cdecl;
  end;

  PPAtkRegistry = ^PAtkRegistry;
  PAtkRegistry = ^TAtkRegistry;
  TAtkRegistry = object(TGObject)
    function get_factory(type_: TGType): PAtkObjectFactory; cdecl; inline;
    function get_factory_type(type_: TGType): TGType; cdecl; inline;
    procedure set_factory_type(type_: TGType; factory_type: TGType); cdecl; inline;
  end;
  TAtkRelation = object(TGObject)
    target1: gpointer;
    relationship: TAtkRelationType;
    function new(targets: PPAtkObject; n_targets: gint; relationship: TAtkRelationType): PAtkRelation; cdecl; inline; static;
    procedure add_target(target: PAtkObject); cdecl; inline;
    function get_relation_type: TAtkRelationType; cdecl; inline;
    function get_target: Pgpointer; cdecl; inline;
    function remove_target(target: PAtkObject): gboolean; cdecl; inline;
    property relation_type:  TAtkRelationType read get_relation_type  { property is writeable but setter not declared } ;
    property target:  Pgpointer read get_target  { property is writeable but setter not declared } ;
  end;

  PPAtkRelationClass = ^PAtkRelationClass;
  PAtkRelationClass = ^TAtkRelationClass;
  TAtkRelationClass = object
    parent: TGObjectClass;
  end;

  PPAtkRelationSetClass = ^PAtkRelationSetClass;
  PAtkRelationSetClass = ^TAtkRelationSetClass;
  TAtkRelationSetClass = object
    parent: TGObjectClass;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
  end;

  PPAtkSelectionIface = ^PAtkSelectionIface;
  PAtkSelectionIface = ^TAtkSelectionIface;
  TAtkSelectionIface = object
    parent: TGTypeInterface;
    add_selection: function(selection: PAtkSelection; i: gint): gboolean; cdecl;
    clear_selection: function(selection: PAtkSelection): gboolean; cdecl;
    ref_selection: function(selection: PAtkSelection; i: gint): PAtkObject; cdecl;
    get_selection_count: function(selection: PAtkSelection): gint; cdecl;
    is_child_selected: function(selection: PAtkSelection; i: gint): gboolean; cdecl;
    remove_selection: function(selection: PAtkSelection; i: gint): gboolean; cdecl;
    select_all_selection: function(selection: PAtkSelection): gboolean; cdecl;
    selection_changed: procedure(selection: PAtkSelection); cdecl;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
  end;

  PPAtkSocket = ^PAtkSocket;
  PAtkSocket = ^TAtkSocket;
  TAtkSocket = object(TAtkObject)
    embedded_plug_id: Pgchar;
    function new: PAtkSocket; cdecl; inline; static;
    procedure embed(plug_id: Pgchar); cdecl; inline;
    function is_occupied: gboolean; cdecl; inline;
  end;

  PPAtkSocketClass = ^PAtkSocketClass;
  PAtkSocketClass = ^TAtkSocketClass;
  TAtkSocketClass = object
    parent_class: TAtkObjectClass;
    embed: procedure(obj: PAtkSocket; plug_id: Pgchar); cdecl;
  end;

  PPAtkStateSetClass = ^PAtkStateSetClass;
  PAtkStateSetClass = ^TAtkStateSetClass;
  TAtkStateSetClass = object
    parent: TGObjectClass;
  end;

  PPAtkStreamableContent = ^PAtkStreamableContent;
  PAtkStreamableContent = ^TAtkStreamableContent;
  TAtkStreamableContent = object
    function get_mime_type(i: gint): Pgchar; cdecl; inline;
    function get_n_mime_types: gint; cdecl; inline;
    function get_stream(mime_type: Pgchar): PGIOChannel; cdecl; inline;
    function get_uri(mime_type: Pgchar): Pgchar; cdecl; inline;
  end;

  PPAtkStreamableContentIface = ^PAtkStreamableContentIface;
  PAtkStreamableContentIface = ^TAtkStreamableContentIface;
  TAtkStreamableContentIface = object
    parent: TGTypeInterface;
    get_n_mime_types: function(streamable: PAtkStreamableContent): gint; cdecl;
    get_mime_type: function(streamable: PAtkStreamableContent; i: gint): Pgchar; cdecl;
    get_stream: function(streamable: PAtkStreamableContent; mime_type: Pgchar): PGIOChannel; cdecl;
    get_uri: function(streamable: PAtkStreamableContent; mime_type: Pgchar): Pgchar; cdecl;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
    pad3: TAtkFunction;
  end;

  PPAtkTableIface = ^PAtkTableIface;
  PAtkTableIface = ^TAtkTableIface;
  TAtkTableIface = object
    parent: TGTypeInterface;
    ref_at: function(table: PAtkTable; row: gint; column: gint): PAtkObject; cdecl;
    get_index_at: function(table: PAtkTable; row: gint; column: gint): gint; cdecl;
    get_column_at_index: function(table: PAtkTable; index_: gint): gint; cdecl;
    get_row_at_index: function(table: PAtkTable; index_: gint): gint; cdecl;
    get_n_columns: function(table: PAtkTable): gint; cdecl;
    get_n_rows: function(table: PAtkTable): gint; cdecl;
    get_column_extent_at: function(table: PAtkTable; row: gint; column: gint): gint; cdecl;
    get_row_extent_at: function(table: PAtkTable; row: gint; column: gint): gint; cdecl;
    get_caption: function(table: PAtkTable): PAtkObject; cdecl;
    get_column_description: function(table: PAtkTable; column: gint): Pgchar; cdecl;
    get_column_header: function(table: PAtkTable; column: gint): PAtkObject; cdecl;
    get_row_description: function(table: PAtkTable; row: gint): Pgchar; cdecl;
    get_row_header: function(table: PAtkTable; row: gint): PAtkObject; cdecl;
    get_summary: function(table: PAtkTable): PAtkObject; cdecl;
    set_caption: procedure(table: PAtkTable; caption: PAtkObject); cdecl;
    set_column_description: procedure(table: PAtkTable; column: gint; description: Pgchar); cdecl;
    set_column_header: procedure(table: PAtkTable; column: gint; header: PAtkObject); cdecl;
    set_row_description: procedure(table: PAtkTable; row: gint; description: Pgchar); cdecl;
    set_row_header: procedure(table: PAtkTable; row: gint; header: PAtkObject); cdecl;
    set_summary: procedure(table: PAtkTable; accessible: PAtkObject); cdecl;
    get_selected_columns: function(table: PAtkTable; selected: PPgint): gint; cdecl;
    get_selected_rows: function(table: PAtkTable; selected: PPgint): gint; cdecl;
    is_column_selected: function(table: PAtkTable; column: gint): gboolean; cdecl;
    is_row_selected: function(table: PAtkTable; row: gint): gboolean; cdecl;
    is_selected: function(table: PAtkTable; row: gint; column: gint): gboolean; cdecl;
    add_row_selection: function(table: PAtkTable; row: gint): gboolean; cdecl;
    remove_row_selection: function(table: PAtkTable; row: gint): gboolean; cdecl;
    add_column_selection: function(table: PAtkTable; column: gint): gboolean; cdecl;
    remove_column_selection: function(table: PAtkTable; column: gint): gboolean; cdecl;
    row_inserted: procedure(table: PAtkTable; row: gint; num_inserted: gint); cdecl;
    column_inserted: procedure(table: PAtkTable; column: gint; num_inserted: gint); cdecl;
    row_deleted: procedure(table: PAtkTable; row: gint; num_deleted: gint); cdecl;
    column_deleted: procedure(table: PAtkTable; column: gint; num_deleted: gint); cdecl;
    row_reordered: procedure(table: PAtkTable); cdecl;
    column_reordered: procedure(table: PAtkTable); cdecl;
    model_changed: procedure(table: PAtkTable); cdecl;
    pad1: TAtkFunction;
    pad2: TAtkFunction;
    pad3: TAtkFunction;
    pad4: TAtkFunction;
  end;

  TAtkTextRectangle = record
    x: gint;
    y: gint;
    width: gint;
    height: gint;
  end;


  TAtkTextRange = object
    bounds: TAtkTextRectangle;
    start_offset: gint;
    end_offset: gint;
    content: Pgchar;
  end;

  PPAtkTextAttribute = ^PAtkTextAttribute;
  PAtkTextAttribute = ^TAtkTextAttribute;

  PPAtkTextIface = ^PAtkTextIface;
  PAtkTextIface = ^TAtkTextIface;
  TAtkTextIface = object
    parent: TGTypeInterface;
    get_text: function(text: PAtkText; start_offset: gint; end_offset: gint): Pgchar; cdecl;
    get_text_after_offset: function(text: PAtkText; offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl;
    get_text_at_offset: function(text: PAtkText; offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl;
    get_character_at_offset: function(text: PAtkText; offset: gint): gunichar; cdecl;
    get_text_before_offset: function(text: PAtkText; offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl;
    get_caret_offset: function(text: PAtkText): gint; cdecl;
    get_run_attributes: function(text: PAtkText; offset: gint; start_offset: Pgint; end_offset: Pgint): PAtkAttributeSet; cdecl;
    get_default_attributes: function(text: PAtkText): PAtkAttributeSet; cdecl;
    get_character_extents: procedure(text: PAtkText; offset: gint; x: Pgint; y: Pgint; width: Pgint; height: Pgint; coords: TAtkCoordType); cdecl;
    get_character_count: function(text: PAtkText): gint; cdecl;
    get_offset_at_point: function(text: PAtkText; x: gint; y: gint; coords: TAtkCoordType): gint; cdecl;
    get_n_selections: function(text: PAtkText): gint; cdecl;
    get_selection: function(text: PAtkText; selection_num: gint; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl;
    add_selection: function(text: PAtkText; start_offset: gint; end_offset: gint): gboolean; cdecl;
    remove_selection: function(text: PAtkText; selection_num: gint): gboolean; cdecl;
    set_selection: function(text: PAtkText; selection_num: gint; start_offset: gint; end_offset: gint): gboolean; cdecl;
    set_caret_offset: function(text: PAtkText; offset: gint): gboolean; cdecl;
    text_changed: procedure(text: PAtkText; position: gint; length: gint); cdecl;
    text_caret_moved: procedure(text: PAtkText; location: gint); cdecl;
    text_selection_changed: procedure(text: PAtkText); cdecl;
    text_attributes_changed: procedure(text: PAtkText); cdecl;
    get_range_extents: procedure(text: PAtkText; start_offset: gint; end_offset: gint; coord_type: TAtkCoordType; rect: PAtkTextRectangle); cdecl;
    get_bounded_ranges: function(text: PAtkText; rect: PAtkTextRectangle; coord_type: TAtkCoordType; x_clip_type: TAtkTextClipType; y_clip_type: TAtkTextClipType): PPAtkTextRange; cdecl;
    pad4: TAtkFunction;
  end;

  PPAtkUtil = ^PAtkUtil;
  PAtkUtil = ^TAtkUtil;
  TAtkUtil = object(TGObject)
  end;

  PPAtkUtilClass = ^PAtkUtilClass;
  PAtkUtilClass = ^TAtkUtilClass;

  PPAtkKeySnoopFunc = ^PAtkKeySnoopFunc;
  PAtkKeySnoopFunc = ^TAtkKeySnoopFunc;
  TAtkUtilClass = object
    parent: TGObjectClass;
    add_global_event_listener: function(listener: TGSignalEmissionHook; event_type: Pgchar): guint; cdecl;
    remove_global_event_listener: procedure(listener_id: guint); cdecl;
    add_key_event_listener: function(listener: TAtkKeySnoopFunc; data: gpointer): guint; cdecl;
    remove_key_event_listener: procedure(listener_id: guint); cdecl;
    get_root: function: PAtkObject; cdecl;
    get_toolkit_name: function: Pgchar; cdecl;
    get_toolkit_version: function: Pgchar; cdecl;
  end;

  PPAtkValue = ^PAtkValue;
  PAtkValue = ^TAtkValue;
  TAtkValue = object
    procedure get_current_value(value: PGValue); cdecl; inline;
    procedure get_maximum_value(value: PGValue); cdecl; inline;
    procedure get_minimum_increment(value: PGValue); cdecl; inline;
    procedure get_minimum_value(value: PGValue); cdecl; inline;
    function set_current_value(value: PGValue): gboolean; cdecl; inline;
  end;

  PPAtkValueIface = ^PAtkValueIface;
  PAtkValueIface = ^TAtkValueIface;
  TAtkValueIface = object
    parent: TGTypeInterface;
    get_current_value: procedure(obj: PAtkValue; value: PGValue); cdecl;
    get_maximum_value: procedure(obj: PAtkValue; value: PGValue); cdecl;
    get_minimum_value: procedure(obj: PAtkValue; value: PGValue); cdecl;
    set_current_value: function(obj: PAtkValue; value: PGValue): gboolean; cdecl;
    get_minimum_increment: procedure(obj: PAtkValue; value: PGValue); cdecl;
    pad1: TAtkFunction;
  end;

  PPAtkWindowIface = ^PAtkWindowIface;
  PAtkWindowIface = ^TAtkWindowIface;
  TAtkWindowIface = object
    parent: TGTypeInterface;
    _padding_dummy: array [0..15] of gpointer;
  end;

  PP_AtkPropertyValues = ^P_AtkPropertyValues;
  P_AtkPropertyValues = ^T_AtkPropertyValues;

  T_AtkPropertyValues = record
    property_name: Pgchar;
    old_value: TGValue;
    new_value: TGValue;
  end;



  PP_AtkRegistry = ^P_AtkRegistry;
  P_AtkRegistry = ^T_AtkRegistry;

  T_AtkRegistry = record
    parent: TGObject;
    factory_type_registry: PGHashTable;
    factory_singleton_cache: PGHashTable;
  end;



  PP_AtkRegistryClass = ^P_AtkRegistryClass;
  P_AtkRegistryClass = ^T_AtkRegistryClass;

  T_AtkRegistryClass = record
    parent_class: TGObjectClass;
  end;



function atk_action_do_action(AAction: PAtkAction; i: gint): gboolean; cdecl; external;
function atk_action_get_description(AAction: PAtkAction; i: gint): Pgchar; cdecl; external;
function atk_action_get_keybinding(AAction: PAtkAction; i: gint): Pgchar; cdecl; external;
function atk_action_get_localized_name(AAction: PAtkAction; i: gint): Pgchar; cdecl; external;
function atk_action_get_n_actions(AAction: PAtkAction): gint; cdecl; external;
function atk_action_get_name(AAction: PAtkAction; i: gint): Pgchar; cdecl; external;
function atk_action_get_type: TGType; cdecl; external;
function atk_action_set_description(AAction: PAtkAction; i: gint; desc: Pgchar): gboolean; cdecl; external;
function atk_add_focus_tracker(focus_tracker: TAtkEventListener): guint; cdecl; external;
function atk_add_global_event_listener(listener: TGSignalEmissionHook; event_type: Pgchar): guint; cdecl; external;
function atk_add_key_event_listener(listener: TAtkKeySnoopFunc; data: gpointer): guint; cdecl; external;
function atk_component_add_focus_handler(AComponent: PAtkComponent; handler: TAtkFocusHandler): guint; cdecl; external;
function atk_component_contains(AComponent: PAtkComponent; x: gint; y: gint; coord_type: TAtkCoordType): gboolean; cdecl; external;
function atk_component_get_alpha(AComponent: PAtkComponent): gdouble; cdecl; external;
function atk_component_get_layer(AComponent: PAtkComponent): TAtkLayer; cdecl; external;
function atk_component_get_mdi_zorder(AComponent: PAtkComponent): gint; cdecl; external;
function atk_component_get_type: TGType; cdecl; external;
function atk_component_grab_focus(AComponent: PAtkComponent): gboolean; cdecl; external;
function atk_component_ref_accessible_at_point(AComponent: PAtkComponent; x: gint; y: gint; coord_type: TAtkCoordType): PGObject; cdecl; external;
function atk_component_set_extents(AComponent: PAtkComponent; x: gint; y: gint; width: gint; height: gint; coord_type: TAtkCoordType): gboolean; cdecl; external;
function atk_component_set_position(AComponent: PAtkComponent; x: gint; y: gint; coord_type: TAtkCoordType): gboolean; cdecl; external;
function atk_component_set_size(AComponent: PAtkComponent; width: gint; height: gint): gboolean; cdecl; external;
function atk_document_get_attribute_value(ADocument: PAtkDocument; attribute_name: Pgchar): Pgchar; cdecl; external;
function atk_document_get_attributes(ADocument: PAtkDocument): PAtkAttributeSet; cdecl; external;
function atk_document_get_document(ADocument: PAtkDocument): gpointer; cdecl; external;
function atk_document_get_document_type(ADocument: PAtkDocument): Pgchar; cdecl; external;
function atk_document_get_locale(ADocument: PAtkDocument): Pgchar; cdecl; external;
function atk_document_get_type: TGType; cdecl; external;
function atk_document_set_attribute_value(ADocument: PAtkDocument; attribute_name: Pgchar; attribute_value: Pgchar): gboolean; cdecl; external;
function atk_editable_text_get_type: TGType; cdecl; external;
function atk_editable_text_set_run_attributes(AEditableText: PAtkEditableText; attrib_set: PAtkAttributeSet; start_offset: gint; end_offset: gint): gboolean; cdecl; external;
function atk_get_default_registry: PAtkRegistry; cdecl; external;
function atk_get_focus_object: PAtkObject; cdecl; external;
function atk_get_root: PAtkObject; cdecl; external;
function atk_get_toolkit_name: Pgchar; cdecl; external;
function atk_get_toolkit_version: Pgchar; cdecl; external;
function atk_get_version: Pgchar; cdecl; external;
function atk_gobject_accessible_for_object(obj: PGObject): PGObject; cdecl; external;
function atk_gobject_accessible_get_object(AGObjectAccessible: PAtkGObjectAccessible): PGObject; cdecl; external;
function atk_gobject_accessible_get_type: TGType; cdecl; external;
function atk_hyperlink_get_end_index(AHyperlink: PAtkHyperlink): gint; cdecl; external;
function atk_hyperlink_get_n_anchors(AHyperlink: PAtkHyperlink): gint; cdecl; external;
function atk_hyperlink_get_object(AHyperlink: PAtkHyperlink; i: gint): PGObject; cdecl; external;
function atk_hyperlink_get_start_index(AHyperlink: PAtkHyperlink): gint; cdecl; external;
function atk_hyperlink_get_type: TGType; cdecl; external;
function atk_hyperlink_get_uri(AHyperlink: PAtkHyperlink; i: gint): Pgchar; cdecl; external;
function atk_hyperlink_impl_get_hyperlink(AHyperlinkImpl: PAtkHyperlinkImpl): PAtkHyperlink; cdecl; external;
function atk_hyperlink_impl_get_type: TGType; cdecl; external;
function atk_hyperlink_is_inline(AHyperlink: PAtkHyperlink): gboolean; cdecl; external;
function atk_hyperlink_is_selected_link(AHyperlink: PAtkHyperlink): gboolean; cdecl; external;
function atk_hyperlink_is_valid(AHyperlink: PAtkHyperlink): gboolean; cdecl; external;
function atk_hypertext_get_link(AHypertext: PAtkHypertext; link_index: gint): PAtkHyperlink; cdecl; external;
function atk_hypertext_get_link_index(AHypertext: PAtkHypertext; char_index: gint): gint; cdecl; external;
function atk_hypertext_get_n_links(AHypertext: PAtkHypertext): gint; cdecl; external;
function atk_hypertext_get_type: TGType; cdecl; external;
function atk_image_get_image_description(AImage: PAtkImage): Pgchar; cdecl; external;
function atk_image_get_image_locale(AImage: PAtkImage): Pgchar; cdecl; external;
function atk_image_get_type: TGType; cdecl; external;
function atk_image_set_image_description(AImage: PAtkImage; description: Pgchar): gboolean; cdecl; external;
function atk_implementor_get_type: TGType; cdecl; external;
function atk_implementor_ref_accessible(AImplementor: PAtkImplementor): PGObject; cdecl; external;
function atk_misc_get_instance: PAtkMisc; cdecl; external;
function atk_misc_get_type: TGType; cdecl; external;
function atk_no_op_object_factory_get_type: TGType; cdecl; external;
function atk_no_op_object_factory_new: PAtkNoOpObjectFactory; cdecl; external;
function atk_no_op_object_get_type: TGType; cdecl; external;
function atk_no_op_object_new(obj: PGObject): PAtkNoOpObject; cdecl; external;
function atk_object_add_relationship(AObject: PAtkObject; relationship: TAtkRelationType; target: PGObject): gboolean; cdecl; external;
function atk_object_connect_property_change_handler(AObject: PAtkObject; handler: PAtkPropertyChangeHandler): guint; cdecl; external;
function atk_object_factory_create_accessible(AObjectFactory: PAtkObjectFactory; obj: PGObject): PAtkObject; cdecl; external;
function atk_object_factory_get_accessible_type(AObjectFactory: PAtkObjectFactory): TGType; cdecl; external;
function atk_object_factory_get_type: TGType; cdecl; external;
function atk_object_get_attributes(AObject: PAtkObject): PAtkAttributeSet; cdecl; external;
function atk_object_get_description(AObject: PAtkObject): Pgchar; cdecl; external;
function atk_object_get_index_in_parent(AObject: PAtkObject): gint; cdecl; external;
function atk_object_get_layer(AObject: PAtkObject): TAtkLayer; cdecl; external;
function atk_object_get_mdi_zorder(AObject: PAtkObject): gint; cdecl; external;
function atk_object_get_n_accessible_children(AObject: PAtkObject): gint; cdecl; external;
function atk_object_get_name(AObject: PAtkObject): Pgchar; cdecl; external;
function atk_object_get_parent(AObject: PAtkObject): PGObject; cdecl; external;
function atk_object_get_role(AObject: PAtkObject): TAtkRole; cdecl; external;
function atk_object_get_type: TGType; cdecl; external;
function atk_object_ref_accessible_child(AObject: PAtkObject; i: gint): PGObject; cdecl; external;
function atk_object_ref_relation_set(AObject: PAtkObject): PAtkRelationSet; cdecl; external;
function atk_object_ref_state_set(AObject: PAtkObject): PAtkStateSet; cdecl; external;
function atk_object_remove_relationship(AObject: PAtkObject; relationship: TAtkRelationType; target: PGObject): gboolean; cdecl; external;
function atk_plug_get_id(APlug: PAtkPlug): Pgchar; cdecl; external;
function atk_plug_get_type: TGType; cdecl; external;
function atk_plug_new: PAtkPlug; cdecl; external;
function atk_rectangle_get_type: TGType; cdecl; external;
function atk_registry_get_factory(ARegistry: PAtkRegistry; type_: TGType): PAtkObjectFactory; cdecl; external;
function atk_registry_get_factory_type(ARegistry: PAtkRegistry; type_: TGType): TGType; cdecl; external;
function atk_registry_get_type: TGType; cdecl; external;
function atk_relation_get_relation_type(ARelation: PAtkRelation): TAtkRelationType; cdecl; external;
function atk_relation_get_target(ARelation: PAtkRelation): Pgpointer; cdecl; external;
function atk_relation_get_type: TGType; cdecl; external;
function atk_relation_new(targets: PPAtkObject; n_targets: gint; relationship: TAtkRelationType): PAtkRelation; cdecl; external;
function atk_relation_remove_target(ARelation: PAtkRelation; target: PAtkObject): gboolean; cdecl; external;
function atk_relation_set_contains(ARelationSet: PAtkRelationSet; relationship: TAtkRelationType): gboolean; cdecl; external;
function atk_relation_set_get_n_relations(ARelationSet: PAtkRelationSet): gint; cdecl; external;
function atk_relation_set_get_relation(ARelationSet: PAtkRelationSet; i: gint): PAtkRelation; cdecl; external;
function atk_relation_set_get_relation_by_type(ARelationSet: PAtkRelationSet; relationship: TAtkRelationType): PAtkRelation; cdecl; external;
function atk_relation_set_get_type: TGType; cdecl; external;
function atk_relation_set_new: PAtkRelationSet; cdecl; external;
function atk_relation_type_for_name(name: Pgchar): TAtkRelationType; cdecl; external;
function atk_relation_type_get_name(type_: TAtkRelationType): Pgchar; cdecl; external;
function atk_relation_type_register(name: Pgchar): TAtkRelationType; cdecl; external;
function atk_role_for_name(name: Pgchar): TAtkRole; cdecl; external;
function atk_role_get_localized_name(role: TAtkRole): Pgchar; cdecl; external;
function atk_role_get_name(role: TAtkRole): Pgchar; cdecl; external;
function atk_role_register(name: Pgchar): TAtkRole; cdecl; external;
function atk_selection_add_selection(ASelection: PAtkSelection; i: gint): gboolean; cdecl; external;
function atk_selection_clear_selection(ASelection: PAtkSelection): gboolean; cdecl; external;
function atk_selection_get_selection_count(ASelection: PAtkSelection): gint; cdecl; external;
function atk_selection_get_type: TGType; cdecl; external;
function atk_selection_is_child_selected(ASelection: PAtkSelection; i: gint): gboolean; cdecl; external;
function atk_selection_ref_selection(ASelection: PAtkSelection; i: gint): PAtkObject; cdecl; external;
function atk_selection_remove_selection(ASelection: PAtkSelection; i: gint): gboolean; cdecl; external;
function atk_selection_select_all_selection(ASelection: PAtkSelection): gboolean; cdecl; external;
function atk_socket_get_type: TGType; cdecl; external;
function atk_socket_is_occupied(ASocket: PAtkSocket): gboolean; cdecl; external;
function atk_socket_new: PAtkSocket; cdecl; external;
function atk_state_set_add_state(AStateSet: PAtkStateSet; type_: TAtkStateType): gboolean; cdecl; external;
function atk_state_set_and_sets(AStateSet: PAtkStateSet; compare_set: PAtkStateSet): PAtkStateSet; cdecl; external;
function atk_state_set_contains_state(AStateSet: PAtkStateSet; type_: TAtkStateType): gboolean; cdecl; external;
function atk_state_set_contains_states(AStateSet: PAtkStateSet; types: PAtkStateType; n_types: gint): gboolean; cdecl; external;
function atk_state_set_get_type: TGType; cdecl; external;
function atk_state_set_is_empty(AStateSet: PAtkStateSet): gboolean; cdecl; external;
function atk_state_set_new: PAtkStateSet; cdecl; external;
function atk_state_set_or_sets(AStateSet: PAtkStateSet; compare_set: PAtkStateSet): PAtkStateSet; cdecl; external;
function atk_state_set_remove_state(AStateSet: PAtkStateSet; type_: TAtkStateType): gboolean; cdecl; external;
function atk_state_set_xor_sets(AStateSet: PAtkStateSet; compare_set: PAtkStateSet): PAtkStateSet; cdecl; external;
function atk_state_type_for_name(name: Pgchar): TAtkStateType; cdecl; external;
function atk_state_type_get_name(type_: TAtkStateType): Pgchar; cdecl; external;
function atk_state_type_register(name: Pgchar): TAtkStateType; cdecl; external;
function atk_streamable_content_get_mime_type(AStreamableContent: PAtkStreamableContent; i: gint): Pgchar; cdecl; external;
function atk_streamable_content_get_n_mime_types(AStreamableContent: PAtkStreamableContent): gint; cdecl; external;
function atk_streamable_content_get_stream(AStreamableContent: PAtkStreamableContent; mime_type: Pgchar): PGIOChannel; cdecl; external;
function atk_streamable_content_get_type: TGType; cdecl; external;
function atk_streamable_content_get_uri(AStreamableContent: PAtkStreamableContent; mime_type: Pgchar): Pgchar; cdecl; external;
function atk_table_add_column_selection(ATable: PAtkTable; column: gint): gboolean; cdecl; external;
function atk_table_add_row_selection(ATable: PAtkTable; row: gint): gboolean; cdecl; external;
function atk_table_get_caption(ATable: PAtkTable): PAtkObject; cdecl; external;
function atk_table_get_column_at_index(ATable: PAtkTable; index_: gint): gint; cdecl; external;
function atk_table_get_column_description(ATable: PAtkTable; column: gint): Pgchar; cdecl; external;
function atk_table_get_column_extent_at(ATable: PAtkTable; row: gint; column: gint): gint; cdecl; external;
function atk_table_get_column_header(ATable: PAtkTable; column: gint): PAtkObject; cdecl; external;
function atk_table_get_index_at(ATable: PAtkTable; row: gint; column: gint): gint; cdecl; external;
function atk_table_get_n_columns(ATable: PAtkTable): gint; cdecl; external;
function atk_table_get_n_rows(ATable: PAtkTable): gint; cdecl; external;
function atk_table_get_row_at_index(ATable: PAtkTable; index_: gint): gint; cdecl; external;
function atk_table_get_row_description(ATable: PAtkTable; row: gint): Pgchar; cdecl; external;
function atk_table_get_row_extent_at(ATable: PAtkTable; row: gint; column: gint): gint; cdecl; external;
function atk_table_get_row_header(ATable: PAtkTable; row: gint): PAtkObject; cdecl; external;
function atk_table_get_selected_columns(ATable: PAtkTable; selected: PPgint): gint; cdecl; external;
function atk_table_get_selected_rows(ATable: PAtkTable; selected: PPgint): gint; cdecl; external;
function atk_table_get_summary(ATable: PAtkTable): PAtkObject; cdecl; external;
function atk_table_get_type: TGType; cdecl; external;
function atk_table_is_column_selected(ATable: PAtkTable; column: gint): gboolean; cdecl; external;
function atk_table_is_row_selected(ATable: PAtkTable; row: gint): gboolean; cdecl; external;
function atk_table_is_selected(ATable: PAtkTable; row: gint; column: gint): gboolean; cdecl; external;
function atk_table_ref_at(ATable: PAtkTable; row: gint; column: gint): PAtkObject; cdecl; external;
function atk_table_remove_column_selection(ATable: PAtkTable; column: gint): gboolean; cdecl; external;
function atk_table_remove_row_selection(ATable: PAtkTable; row: gint): gboolean; cdecl; external;
function atk_text_add_selection(AText: PAtkText; start_offset: gint; end_offset: gint): gboolean; cdecl; external;
function atk_text_attribute_for_name(name: Pgchar): TAtkTextAttribute; cdecl; external;
function atk_text_attribute_get_name(attr: TAtkTextAttribute): Pgchar; cdecl; external;
function atk_text_attribute_get_value(attr: TAtkTextAttribute; index_: gint): Pgchar; cdecl; external;
function atk_text_attribute_register(name: Pgchar): TAtkTextAttribute; cdecl; external;
function atk_text_get_bounded_ranges(AText: PAtkText; rect: PAtkTextRectangle; coord_type: TAtkCoordType; x_clip_type: TAtkTextClipType; y_clip_type: TAtkTextClipType): PPAtkTextRange; cdecl; external;
function atk_text_get_caret_offset(AText: PAtkText): gint; cdecl; external;
function atk_text_get_character_at_offset(AText: PAtkText; offset: gint): gunichar; cdecl; external;
function atk_text_get_character_count(AText: PAtkText): gint; cdecl; external;
function atk_text_get_default_attributes(AText: PAtkText): PAtkAttributeSet; cdecl; external;
function atk_text_get_n_selections(AText: PAtkText): gint; cdecl; external;
function atk_text_get_offset_at_point(AText: PAtkText; x: gint; y: gint; coords: TAtkCoordType): gint; cdecl; external;
function atk_text_get_run_attributes(AText: PAtkText; offset: gint; start_offset: Pgint; end_offset: Pgint): PAtkAttributeSet; cdecl; external;
function atk_text_get_selection(AText: PAtkText; selection_num: gint; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl; external;
function atk_text_get_text(AText: PAtkText; start_offset: gint; end_offset: gint): Pgchar; cdecl; external;
function atk_text_get_text_after_offset(AText: PAtkText; offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl; external;
function atk_text_get_text_at_offset(AText: PAtkText; offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl; external;
function atk_text_get_text_before_offset(AText: PAtkText; offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl; external;
function atk_text_get_type: TGType; cdecl; external;
function atk_text_range_get_type: TGType; cdecl; external;
function atk_text_remove_selection(AText: PAtkText; selection_num: gint): gboolean; cdecl; external;
function atk_text_set_caret_offset(AText: PAtkText; offset: gint): gboolean; cdecl; external;
function atk_text_set_selection(AText: PAtkText; selection_num: gint; start_offset: gint; end_offset: gint): gboolean; cdecl; external;
function atk_util_get_type: TGType; cdecl; external;
function atk_value_get_type: TGType; cdecl; external;
function atk_value_set_current_value(AValue: PAtkValue; value: PGValue): gboolean; cdecl; external;
function atk_window_get_type: TGType; cdecl; external;
procedure atk_attribute_set_free(attrib_set: PAtkAttributeSet); cdecl; external;
procedure atk_component_get_extents(AComponent: PAtkComponent; x: Pgint; y: Pgint; width: Pgint; height: Pgint; coord_type: TAtkCoordType); cdecl; external;
procedure atk_component_get_position(AComponent: PAtkComponent; x: Pgint; y: Pgint; coord_type: TAtkCoordType); cdecl; external;
procedure atk_component_get_size(AComponent: PAtkComponent; width: Pgint; height: Pgint); cdecl; external;
procedure atk_component_remove_focus_handler(AComponent: PAtkComponent; handler_id: guint); cdecl; external;
procedure atk_editable_text_copy_text(AEditableText: PAtkEditableText; start_pos: gint; end_pos: gint); cdecl; external;
procedure atk_editable_text_cut_text(AEditableText: PAtkEditableText; start_pos: gint; end_pos: gint); cdecl; external;
procedure atk_editable_text_delete_text(AEditableText: PAtkEditableText; start_pos: gint; end_pos: gint); cdecl; external;
procedure atk_editable_text_insert_text(AEditableText: PAtkEditableText; string_: Pgchar; length: gint; position: Pgint); cdecl; external;
procedure atk_editable_text_paste_text(AEditableText: PAtkEditableText; position: gint); cdecl; external;
procedure atk_editable_text_set_text_contents(AEditableText: PAtkEditableText; string_: Pgchar); cdecl; external;
procedure atk_focus_tracker_init(init: TAtkEventListenerInit); cdecl; external;
procedure atk_focus_tracker_notify(object_: PAtkObject); cdecl; external;
procedure atk_image_get_image_position(AImage: PAtkImage; x: Pgint; y: Pgint; coord_type: TAtkCoordType); cdecl; external;
procedure atk_image_get_image_size(AImage: PAtkImage; width: Pgint; height: Pgint); cdecl; external;
procedure atk_misc_threads_enter(AMisc: PAtkMisc); cdecl; external;
procedure atk_misc_threads_leave(AMisc: PAtkMisc); cdecl; external;
procedure atk_object_factory_invalidate(AObjectFactory: PAtkObjectFactory); cdecl; external;
procedure atk_object_initialize(AObject: PAtkObject; data: gpointer); cdecl; external;
procedure atk_object_notify_state_change(AObject: PAtkObject; state: TAtkState; value: gboolean); cdecl; external;
procedure atk_object_remove_property_change_handler(AObject: PAtkObject; handler_id: guint); cdecl; external;
procedure atk_object_set_description(AObject: PAtkObject; description: Pgchar); cdecl; external;
procedure atk_object_set_name(AObject: PAtkObject; name: Pgchar); cdecl; external;
procedure atk_object_set_parent(AObject: PAtkObject; parent: PGObject); cdecl; external;
procedure atk_object_set_role(AObject: PAtkObject; role: TAtkRole); cdecl; external;
procedure atk_registry_set_factory_type(ARegistry: PAtkRegistry; type_: TGType; factory_type: TGType); cdecl; external;
procedure atk_relation_add_target(ARelation: PAtkRelation; target: PAtkObject); cdecl; external;
procedure atk_relation_set_add(ARelationSet: PAtkRelationSet; relation: PAtkRelation); cdecl; external;
procedure atk_relation_set_add_relation_by_type(ARelationSet: PAtkRelationSet; relationship: TAtkRelationType; target: PAtkObject); cdecl; external;
procedure atk_relation_set_remove(ARelationSet: PAtkRelationSet; relation: PAtkRelation); cdecl; external;
procedure atk_remove_focus_tracker(tracker_id: guint); cdecl; external;
procedure atk_remove_global_event_listener(listener_id: guint); cdecl; external;
procedure atk_remove_key_event_listener(listener_id: guint); cdecl; external;
procedure atk_socket_embed(ASocket: PAtkSocket; plug_id: Pgchar); cdecl; external;
procedure atk_state_set_add_states(AStateSet: PAtkStateSet; types: PAtkStateType; n_types: gint); cdecl; external;
procedure atk_state_set_clear_states(AStateSet: PAtkStateSet); cdecl; external;
procedure atk_table_set_caption(ATable: PAtkTable; caption: PAtkObject); cdecl; external;
procedure atk_table_set_column_description(ATable: PAtkTable; column: gint; description: Pgchar); cdecl; external;
procedure atk_table_set_column_header(ATable: PAtkTable; column: gint; header: PAtkObject); cdecl; external;
procedure atk_table_set_row_description(ATable: PAtkTable; row: gint; description: Pgchar); cdecl; external;
procedure atk_table_set_row_header(ATable: PAtkTable; row: gint; header: PAtkObject); cdecl; external;
procedure atk_table_set_summary(ATable: PAtkTable; accessible: PAtkObject); cdecl; external;
procedure atk_text_free_ranges(ranges: PPAtkTextRange); cdecl; external;
procedure atk_text_get_character_extents(AText: PAtkText; offset: gint; x: Pgint; y: Pgint; width: Pgint; height: Pgint; coords: TAtkCoordType); cdecl; external;
procedure atk_text_get_range_extents(AText: PAtkText; start_offset: gint; end_offset: gint; coord_type: TAtkCoordType; rect: PAtkTextRectangle); cdecl; external;
procedure atk_value_get_current_value(AValue: PAtkValue; value: PGValue); cdecl; external;
procedure atk_value_get_maximum_value(AValue: PAtkValue; value: PGValue); cdecl; external;
procedure atk_value_get_minimum_increment(AValue: PAtkValue; value: PGValue); cdecl; external;
procedure atk_value_get_minimum_value(AValue: PAtkValue; value: PGValue); cdecl; external;
implementation
function TAtkAction.do_action(i: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_action_do_action(@self, i);
end;

function TAtkAction.get_description(i: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_action_get_description(@self, i);
end;

function TAtkAction.get_keybinding(i: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_action_get_keybinding(@self, i);
end;

function TAtkAction.get_localized_name(i: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_action_get_localized_name(@self, i);
end;

function TAtkAction.get_n_actions: gint; cdecl;
begin
  Result := Atk1.atk_action_get_n_actions(@self);
end;

function TAtkAction.get_name(i: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_action_get_name(@self, i);
end;

function TAtkAction.set_description(i: gint; desc: Pgchar): gboolean; cdecl;
begin
  Result := Atk1.atk_action_set_description(@self, i, desc);
end;

function TAtkComponent.add_focus_handler(handler: TAtkFocusHandler): guint; cdecl;
begin
  Result := Atk1.atk_component_add_focus_handler(@self, handler);
end;

function TAtkComponent.contains(x: gint; y: gint; coord_type: TAtkCoordType): gboolean; cdecl;
begin
  Result := Atk1.atk_component_contains(@self, x, y, coord_type);
end;

function TAtkComponent.get_alpha: gdouble; cdecl;
begin
  Result := Atk1.atk_component_get_alpha(@self);
end;

procedure TAtkComponent.get_extents(x: Pgint; y: Pgint; width: Pgint; height: Pgint; coord_type: TAtkCoordType); cdecl;
begin
  Atk1.atk_component_get_extents(@self, x, y, width, height, coord_type);
end;

function TAtkComponent.get_layer: TAtkLayer; cdecl;
begin
  Result := Atk1.atk_component_get_layer(@self);
end;

function TAtkComponent.get_mdi_zorder: gint; cdecl;
begin
  Result := Atk1.atk_component_get_mdi_zorder(@self);
end;

procedure TAtkComponent.get_position(x: Pgint; y: Pgint; coord_type: TAtkCoordType); cdecl;
begin
  Atk1.atk_component_get_position(@self, x, y, coord_type);
end;

procedure TAtkComponent.get_size(width: Pgint; height: Pgint); cdecl;
begin
  Atk1.atk_component_get_size(@self, width, height);
end;

function TAtkComponent.grab_focus: gboolean; cdecl;
begin
  Result := Atk1.atk_component_grab_focus(@self);
end;

function TAtkComponent.ref_accessible_at_point(x: gint; y: gint; coord_type: TAtkCoordType): PGObject; cdecl;
begin
  Result := Atk1.atk_component_ref_accessible_at_point(@self, x, y, coord_type);
end;

procedure TAtkComponent.remove_focus_handler(handler_id: guint); cdecl;
begin
  Atk1.atk_component_remove_focus_handler(@self, handler_id);
end;

function TAtkComponent.set_extents(x: gint; y: gint; width: gint; height: gint; coord_type: TAtkCoordType): gboolean; cdecl;
begin
  Result := Atk1.atk_component_set_extents(@self, x, y, width, height, coord_type);
end;

function TAtkComponent.set_position(x: gint; y: gint; coord_type: TAtkCoordType): gboolean; cdecl;
begin
  Result := Atk1.atk_component_set_position(@self, x, y, coord_type);
end;

function TAtkComponent.set_size(width: gint; height: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_component_set_size(@self, width, height);
end;

function TAtkDocument.get_attribute_value(attribute_name: Pgchar): Pgchar; cdecl;
begin
  Result := Atk1.atk_document_get_attribute_value(@self, attribute_name);
end;

function TAtkDocument.get_attributes: PAtkAttributeSet; cdecl;
begin
  Result := Atk1.atk_document_get_attributes(@self);
end;

function TAtkDocument.get_document: gpointer; cdecl;
begin
  Result := Atk1.atk_document_get_document(@self);
end;

function TAtkDocument.get_document_type: Pgchar; cdecl;
begin
  Result := Atk1.atk_document_get_document_type(@self);
end;

function TAtkDocument.get_locale: Pgchar; cdecl;
begin
  Result := Atk1.atk_document_get_locale(@self);
end;

function TAtkDocument.set_attribute_value(attribute_name: Pgchar; attribute_value: Pgchar): gboolean; cdecl;
begin
  Result := Atk1.atk_document_set_attribute_value(@self, attribute_name, attribute_value);
end;

procedure TAtkEditableText.copy_text(start_pos: gint; end_pos: gint); cdecl;
begin
  Atk1.atk_editable_text_copy_text(@self, start_pos, end_pos);
end;

procedure TAtkEditableText.cut_text(start_pos: gint; end_pos: gint); cdecl;
begin
  Atk1.atk_editable_text_cut_text(@self, start_pos, end_pos);
end;

procedure TAtkEditableText.delete_text(start_pos: gint; end_pos: gint); cdecl;
begin
  Atk1.atk_editable_text_delete_text(@self, start_pos, end_pos);
end;

procedure TAtkEditableText.insert_text(string_: Pgchar; length: gint; position: Pgint); cdecl;
begin
  Atk1.atk_editable_text_insert_text(@self, string_, length, position);
end;

procedure TAtkEditableText.paste_text(position: gint); cdecl;
begin
  Atk1.atk_editable_text_paste_text(@self, position);
end;

function TAtkEditableText.set_run_attributes(attrib_set: PAtkAttributeSet; start_offset: gint; end_offset: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_editable_text_set_run_attributes(@self, attrib_set, start_offset, end_offset);
end;

procedure TAtkEditableText.set_text_contents(string_: Pgchar); cdecl;
begin
  Atk1.atk_editable_text_set_text_contents(@self, string_);
end;

function TAtkGObjectAccessible.for_object(obj: PGObject): PGObject; cdecl;
begin
  Result := Atk1.atk_gobject_accessible_for_object(obj);
end;

function TAtkGObjectAccessible.get_object: PGObject; cdecl;
begin
  Result := Atk1.atk_gobject_accessible_get_object(@self);
end;

function TAtkHyperlink.get_end_index: gint; cdecl;
begin
  Result := Atk1.atk_hyperlink_get_end_index(@self);
end;

function TAtkHyperlink.get_n_anchors: gint; cdecl;
begin
  Result := Atk1.atk_hyperlink_get_n_anchors(@self);
end;

function TAtkHyperlink.get_object(i: gint): PGObject; cdecl;
begin
  Result := Atk1.atk_hyperlink_get_object(@self, i);
end;

function TAtkHyperlink.get_start_index: gint; cdecl;
begin
  Result := Atk1.atk_hyperlink_get_start_index(@self);
end;

function TAtkHyperlink.get_uri(i: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_hyperlink_get_uri(@self, i);
end;

function TAtkHyperlink.is_inline: gboolean; cdecl;
begin
  Result := Atk1.atk_hyperlink_is_inline(@self);
end;

function TAtkHyperlink.is_selected_link: gboolean; cdecl;
begin
  Result := Atk1.atk_hyperlink_is_selected_link(@self);
end;

function TAtkHyperlink.is_valid: gboolean; cdecl;
begin
  Result := Atk1.atk_hyperlink_is_valid(@self);
end;

function TAtkHyperlinkImpl.get_hyperlink: PAtkHyperlink; cdecl;
begin
  Result := Atk1.atk_hyperlink_impl_get_hyperlink(@self);
end;


function TAtkHypertext.get_link(link_index: gint): PAtkHyperlink; cdecl;
begin
  Result := Atk1.atk_hypertext_get_link(@self, link_index);
end;

function TAtkHypertext.get_link_index(char_index: gint): gint; cdecl;
begin
  Result := Atk1.atk_hypertext_get_link_index(@self, char_index);
end;

function TAtkHypertext.get_n_links: gint; cdecl;
begin
  Result := Atk1.atk_hypertext_get_n_links(@self);
end;

function TAtkImage.get_image_description: Pgchar; cdecl;
begin
  Result := Atk1.atk_image_get_image_description(@self);
end;

function TAtkImage.get_image_locale: Pgchar; cdecl;
begin
  Result := Atk1.atk_image_get_image_locale(@self);
end;

procedure TAtkImage.get_image_position(x: Pgint; y: Pgint; coord_type: TAtkCoordType); cdecl;
begin
  Atk1.atk_image_get_image_position(@self, x, y, coord_type);
end;

procedure TAtkImage.get_image_size(width: Pgint; height: Pgint); cdecl;
begin
  Atk1.atk_image_get_image_size(@self, width, height);
end;

function TAtkImage.set_image_description(description: Pgchar): gboolean; cdecl;
begin
  Result := Atk1.atk_image_set_image_description(@self, description);
end;

function TAtkImplementor.ref_accessible: PGObject; cdecl;
begin
  Result := Atk1.atk_implementor_ref_accessible(@self);
end;

function TAtkMisc.get_instance: PAtkMisc; cdecl;
begin
  Result := Atk1.atk_misc_get_instance();
end;

procedure TAtkMisc.threads_enter; cdecl;
begin
  Atk1.atk_misc_threads_enter(@self);
end;

procedure TAtkMisc.threads_leave; cdecl;
begin
  Atk1.atk_misc_threads_leave(@self);
end;

function TAtkSelection.add_selection(i: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_selection_add_selection(@self, i);
end;

function TAtkSelection.clear_selection: gboolean; cdecl;
begin
  Result := Atk1.atk_selection_clear_selection(@self);
end;

function TAtkSelection.get_selection_count: gint; cdecl;
begin
  Result := Atk1.atk_selection_get_selection_count(@self);
end;

function TAtkSelection.is_child_selected(i: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_selection_is_child_selected(@self, i);
end;

function TAtkSelection.ref_selection(i: gint): PAtkObject; cdecl;
begin
  Result := Atk1.atk_selection_ref_selection(@self, i);
end;

function TAtkSelection.remove_selection(i: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_selection_remove_selection(@self, i);
end;

function TAtkSelection.select_all_selection: gboolean; cdecl;
begin
  Result := Atk1.atk_selection_select_all_selection(@self);
end;

function TAtkTable.add_column_selection(column: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_table_add_column_selection(@self, column);
end;

function TAtkTable.add_row_selection(row: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_table_add_row_selection(@self, row);
end;

function TAtkTable.get_caption: PAtkObject; cdecl;
begin
  Result := Atk1.atk_table_get_caption(@self);
end;

function TAtkTable.get_column_at_index(index_: gint): gint; cdecl;
begin
  Result := Atk1.atk_table_get_column_at_index(@self, index_);
end;

function TAtkTable.get_column_description(column: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_table_get_column_description(@self, column);
end;

function TAtkTable.get_column_extent_at(row: gint; column: gint): gint; cdecl;
begin
  Result := Atk1.atk_table_get_column_extent_at(@self, row, column);
end;

function TAtkTable.get_column_header(column: gint): PAtkObject; cdecl;
begin
  Result := Atk1.atk_table_get_column_header(@self, column);
end;

function TAtkTable.get_index_at(row: gint; column: gint): gint; cdecl;
begin
  Result := Atk1.atk_table_get_index_at(@self, row, column);
end;

function TAtkTable.get_n_columns: gint; cdecl;
begin
  Result := Atk1.atk_table_get_n_columns(@self);
end;

function TAtkTable.get_n_rows: gint; cdecl;
begin
  Result := Atk1.atk_table_get_n_rows(@self);
end;

function TAtkTable.get_row_at_index(index_: gint): gint; cdecl;
begin
  Result := Atk1.atk_table_get_row_at_index(@self, index_);
end;

function TAtkTable.get_row_description(row: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_table_get_row_description(@self, row);
end;

function TAtkTable.get_row_extent_at(row: gint; column: gint): gint; cdecl;
begin
  Result := Atk1.atk_table_get_row_extent_at(@self, row, column);
end;

function TAtkTable.get_row_header(row: gint): PAtkObject; cdecl;
begin
  Result := Atk1.atk_table_get_row_header(@self, row);
end;

function TAtkTable.get_selected_columns(selected: PPgint): gint; cdecl;
begin
  Result := Atk1.atk_table_get_selected_columns(@self, selected);
end;

function TAtkTable.get_selected_rows(selected: PPgint): gint; cdecl;
begin
  Result := Atk1.atk_table_get_selected_rows(@self, selected);
end;

function TAtkTable.get_summary: PAtkObject; cdecl;
begin
  Result := Atk1.atk_table_get_summary(@self);
end;

function TAtkTable.is_column_selected(column: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_table_is_column_selected(@self, column);
end;

function TAtkTable.is_row_selected(row: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_table_is_row_selected(@self, row);
end;

function TAtkTable.is_selected(row: gint; column: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_table_is_selected(@self, row, column);
end;

function TAtkTable.ref_at(row: gint; column: gint): PAtkObject; cdecl;
begin
  Result := Atk1.atk_table_ref_at(@self, row, column);
end;

function TAtkTable.remove_column_selection(column: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_table_remove_column_selection(@self, column);
end;

function TAtkTable.remove_row_selection(row: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_table_remove_row_selection(@self, row);
end;

procedure TAtkTable.set_caption(caption: PAtkObject); cdecl;
begin
  Atk1.atk_table_set_caption(@self, caption);
end;

procedure TAtkTable.set_column_description(column: gint; description: Pgchar); cdecl;
begin
  Atk1.atk_table_set_column_description(@self, column, description);
end;

procedure TAtkTable.set_column_header(column: gint; header: PAtkObject); cdecl;
begin
  Atk1.atk_table_set_column_header(@self, column, header);
end;

procedure TAtkTable.set_row_description(row: gint; description: Pgchar); cdecl;
begin
  Atk1.atk_table_set_row_description(@self, row, description);
end;

procedure TAtkTable.set_row_header(row: gint; header: PAtkObject); cdecl;
begin
  Atk1.atk_table_set_row_header(@self, row, header);
end;

procedure TAtkTable.set_summary(accessible: PAtkObject); cdecl;
begin
  Atk1.atk_table_set_summary(@self, accessible);
end;

function TAtkText.add_selection(start_offset: gint; end_offset: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_text_add_selection(@self, start_offset, end_offset);
end;

function TAtkText.get_bounded_ranges(rect: PAtkTextRectangle; coord_type: TAtkCoordType; x_clip_type: TAtkTextClipType; y_clip_type: TAtkTextClipType): PPAtkTextRange; cdecl;
begin
  Result := Atk1.atk_text_get_bounded_ranges(@self, rect, coord_type, x_clip_type, y_clip_type);
end;

function TAtkText.get_caret_offset: gint; cdecl;
begin
  Result := Atk1.atk_text_get_caret_offset(@self);
end;

function TAtkText.get_character_at_offset(offset: gint): gunichar; cdecl;
begin
  Result := Atk1.atk_text_get_character_at_offset(@self, offset);
end;

function TAtkText.get_character_count: gint; cdecl;
begin
  Result := Atk1.atk_text_get_character_count(@self);
end;

procedure TAtkText.get_character_extents(offset: gint; x: Pgint; y: Pgint; width: Pgint; height: Pgint; coords: TAtkCoordType); cdecl;
begin
  Atk1.atk_text_get_character_extents(@self, offset, x, y, width, height, coords);
end;

function TAtkText.get_default_attributes: PAtkAttributeSet; cdecl;
begin
  Result := Atk1.atk_text_get_default_attributes(@self);
end;

function TAtkText.get_n_selections: gint; cdecl;
begin
  Result := Atk1.atk_text_get_n_selections(@self);
end;

function TAtkText.get_offset_at_point(x: gint; y: gint; coords: TAtkCoordType): gint; cdecl;
begin
  Result := Atk1.atk_text_get_offset_at_point(@self, x, y, coords);
end;

procedure TAtkText.get_range_extents(start_offset: gint; end_offset: gint; coord_type: TAtkCoordType; rect: PAtkTextRectangle); cdecl;
begin
  Atk1.atk_text_get_range_extents(@self, start_offset, end_offset, coord_type, rect);
end;

function TAtkText.get_run_attributes(offset: gint; start_offset: Pgint; end_offset: Pgint): PAtkAttributeSet; cdecl;
begin
  Result := Atk1.atk_text_get_run_attributes(@self, offset, start_offset, end_offset);
end;

function TAtkText.get_selection(selection_num: gint; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl;
begin
  Result := Atk1.atk_text_get_selection(@self, selection_num, start_offset, end_offset);
end;

function TAtkText.get_text(start_offset: gint; end_offset: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_text_get_text(@self, start_offset, end_offset);
end;

function TAtkText.get_text_after_offset(offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl;
begin
  Result := Atk1.atk_text_get_text_after_offset(@self, offset, boundary_type, start_offset, end_offset);
end;

function TAtkText.get_text_at_offset(offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl;
begin
  Result := Atk1.atk_text_get_text_at_offset(@self, offset, boundary_type, start_offset, end_offset);
end;

function TAtkText.get_text_before_offset(offset: gint; boundary_type: TAtkTextBoundary; start_offset: Pgint; end_offset: Pgint): Pgchar; cdecl;
begin
  Result := Atk1.atk_text_get_text_before_offset(@self, offset, boundary_type, start_offset, end_offset);
end;

function TAtkText.remove_selection(selection_num: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_text_remove_selection(@self, selection_num);
end;

function TAtkText.set_caret_offset(offset: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_text_set_caret_offset(@self, offset);
end;

function TAtkText.set_selection(selection_num: gint; start_offset: gint; end_offset: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_text_set_selection(@self, selection_num, start_offset, end_offset);
end;

function TAtkNoOpObject.new(obj: PGObject): PAtkNoOpObject; cdecl;
begin
  Result := Atk1.atk_no_op_object_new(obj);
end;

function TAtkObjectFactory.create_accessible(obj: PGObject): PAtkObject; cdecl;
begin
  Result := Atk1.atk_object_factory_create_accessible(@self, obj);
end;

function TAtkObjectFactory.get_accessible_type: TGType; cdecl;
begin
  Result := Atk1.atk_object_factory_get_accessible_type(@self);
end;

procedure TAtkObjectFactory.invalidate; cdecl;
begin
  Atk1.atk_object_factory_invalidate(@self);
end;

function TAtkNoOpObjectFactory.new: PAtkNoOpObjectFactory; cdecl;
begin
  Result := Atk1.atk_no_op_object_factory_new();
end;

function TAtkRelationSet.new: PAtkRelationSet; cdecl;
begin
  Result := Atk1.atk_relation_set_new();
end;

procedure TAtkRelationSet.add(relation: PAtkRelation); cdecl;
begin
  Atk1.atk_relation_set_add(@self, relation);
end;

procedure TAtkRelationSet.add_relation_by_type(relationship: TAtkRelationType; target: PAtkObject); cdecl;
begin
  Atk1.atk_relation_set_add_relation_by_type(@self, relationship, target);
end;

function TAtkRelationSet.contains(relationship: TAtkRelationType): gboolean; cdecl;
begin
  Result := Atk1.atk_relation_set_contains(@self, relationship);
end;

function TAtkRelationSet.get_n_relations: gint; cdecl;
begin
  Result := Atk1.atk_relation_set_get_n_relations(@self);
end;

function TAtkRelationSet.get_relation(i: gint): PAtkRelation; cdecl;
begin
  Result := Atk1.atk_relation_set_get_relation(@self, i);
end;

function TAtkRelationSet.get_relation_by_type(relationship: TAtkRelationType): PAtkRelation; cdecl;
begin
  Result := Atk1.atk_relation_set_get_relation_by_type(@self, relationship);
end;

procedure TAtkRelationSet.remove(relation: PAtkRelation); cdecl;
begin
  Atk1.atk_relation_set_remove(@self, relation);
end;

function TAtkStateSet.new: PAtkStateSet; cdecl;
begin
  Result := Atk1.atk_state_set_new();
end;

function TAtkStateSet.add_state(type_: TAtkStateType): gboolean; cdecl;
begin
  Result := Atk1.atk_state_set_add_state(@self, type_);
end;

procedure TAtkStateSet.add_states(types: PAtkStateType; n_types: gint); cdecl;
begin
  Atk1.atk_state_set_add_states(@self, types, n_types);
end;

function TAtkStateSet.and_sets(compare_set: PAtkStateSet): PAtkStateSet; cdecl;
begin
  Result := Atk1.atk_state_set_and_sets(@self, compare_set);
end;

procedure TAtkStateSet.clear_states; cdecl;
begin
  Atk1.atk_state_set_clear_states(@self);
end;

function TAtkStateSet.contains_state(type_: TAtkStateType): gboolean; cdecl;
begin
  Result := Atk1.atk_state_set_contains_state(@self, type_);
end;

function TAtkStateSet.contains_states(types: PAtkStateType; n_types: gint): gboolean; cdecl;
begin
  Result := Atk1.atk_state_set_contains_states(@self, types, n_types);
end;

function TAtkStateSet.is_empty: gboolean; cdecl;
begin
  Result := Atk1.atk_state_set_is_empty(@self);
end;

function TAtkStateSet.or_sets(compare_set: PAtkStateSet): PAtkStateSet; cdecl;
begin
  Result := Atk1.atk_state_set_or_sets(@self, compare_set);
end;

function TAtkStateSet.remove_state(type_: TAtkStateType): gboolean; cdecl;
begin
  Result := Atk1.atk_state_set_remove_state(@self, type_);
end;

function TAtkStateSet.xor_sets(compare_set: PAtkStateSet): PAtkStateSet; cdecl;
begin
  Result := Atk1.atk_state_set_xor_sets(@self, compare_set);
end;

function TAtkObject.add_relationship(relationship: TAtkRelationType; target: PGObject): gboolean; cdecl;
begin
  Result := Atk1.atk_object_add_relationship(@self, relationship, target);
end;

function TAtkObject.connect_property_change_handler(handler: PAtkPropertyChangeHandler): guint; cdecl;
begin
  Result := Atk1.atk_object_connect_property_change_handler(@self, handler);
end;

function TAtkObject.get_attributes: PAtkAttributeSet; cdecl;
begin
  Result := Atk1.atk_object_get_attributes(@self);
end;

function TAtkObject.get_description: Pgchar; cdecl;
begin
  Result := Atk1.atk_object_get_description(@self);
end;

function TAtkObject.get_index_in_parent: gint; cdecl;
begin
  Result := Atk1.atk_object_get_index_in_parent(@self);
end;

function TAtkObject.get_layer: TAtkLayer; cdecl;
begin
  Result := Atk1.atk_object_get_layer(@self);
end;

function TAtkObject.get_mdi_zorder: gint; cdecl;
begin
  Result := Atk1.atk_object_get_mdi_zorder(@self);
end;

function TAtkObject.get_n_accessible_children: gint; cdecl;
begin
  Result := Atk1.atk_object_get_n_accessible_children(@self);
end;

function TAtkObject.get_name: Pgchar; cdecl;
begin
  Result := Atk1.atk_object_get_name(@self);
end;

function TAtkObject.get_parent: PGObject; cdecl;
begin
  Result := Atk1.atk_object_get_parent(@self);
end;

function TAtkObject.get_role: TAtkRole; cdecl;
begin
  Result := Atk1.atk_object_get_role(@self);
end;

procedure TAtkObject.initialize(data: gpointer); cdecl;
begin
  Atk1.atk_object_initialize(@self, data);
end;

procedure TAtkObject.notify_state_change(state: TAtkState; value: gboolean); cdecl;
begin
  Atk1.atk_object_notify_state_change(@self, state, value);
end;

function TAtkObject.ref_accessible_child(i: gint): PGObject; cdecl;
begin
  Result := Atk1.atk_object_ref_accessible_child(@self, i);
end;

function TAtkObject.ref_relation_set: PAtkRelationSet; cdecl;
begin
  Result := Atk1.atk_object_ref_relation_set(@self);
end;

function TAtkObject.ref_state_set: PAtkStateSet; cdecl;
begin
  Result := Atk1.atk_object_ref_state_set(@self);
end;

procedure TAtkObject.remove_property_change_handler(handler_id: guint); cdecl;
begin
  Atk1.atk_object_remove_property_change_handler(@self, handler_id);
end;

function TAtkObject.remove_relationship(relationship: TAtkRelationType; target: PGObject): gboolean; cdecl;
begin
  Result := Atk1.atk_object_remove_relationship(@self, relationship, target);
end;

procedure TAtkObject.set_description(description: Pgchar); cdecl;
begin
  Atk1.atk_object_set_description(@self, description);
end;

procedure TAtkObject.set_name(name: Pgchar); cdecl;
begin
  Atk1.atk_object_set_name(@self, name);
end;

procedure TAtkObject.set_parent(parent: PGObject); cdecl;
begin
  Atk1.atk_object_set_parent(@self, parent);
end;

procedure TAtkObject.set_role(role: TAtkRole); cdecl;
begin
  Atk1.atk_object_set_role(@self, role);
end;

function TAtkPlug.new: PAtkPlug; cdecl;
begin
  Result := Atk1.atk_plug_new();
end;

function TAtkPlug.get_id: Pgchar; cdecl;
begin
  Result := Atk1.atk_plug_get_id(@self);
end;

function TAtkRegistry.get_factory(type_: TGType): PAtkObjectFactory; cdecl;
begin
  Result := Atk1.atk_registry_get_factory(@self, type_);
end;

function TAtkRegistry.get_factory_type(type_: TGType): TGType; cdecl;
begin
  Result := Atk1.atk_registry_get_factory_type(@self, type_);
end;

procedure TAtkRegistry.set_factory_type(type_: TGType; factory_type: TGType); cdecl;
begin
  Atk1.atk_registry_set_factory_type(@self, type_, factory_type);
end;

function TAtkRelation.new(targets: PPAtkObject; n_targets: gint; relationship: TAtkRelationType): PAtkRelation; cdecl;
begin
  Result := Atk1.atk_relation_new(targets, n_targets, relationship);
end;

procedure TAtkRelation.add_target(target: PAtkObject); cdecl;
begin
  Atk1.atk_relation_add_target(@self, target);
end;

function TAtkRelation.get_relation_type: TAtkRelationType; cdecl;
begin
  Result := Atk1.atk_relation_get_relation_type(@self);
end;

function TAtkRelation.get_target: Pgpointer; cdecl;
begin
  Result := Atk1.atk_relation_get_target(@self);
end;

function TAtkRelation.remove_target(target: PAtkObject): gboolean; cdecl;
begin
  Result := Atk1.atk_relation_remove_target(@self, target);
end;

function TAtkSocket.new: PAtkSocket; cdecl;
begin
  Result := Atk1.atk_socket_new();
end;

procedure TAtkSocket.embed(plug_id: Pgchar); cdecl;
begin
  Atk1.atk_socket_embed(@self, plug_id);
end;

function TAtkSocket.is_occupied: gboolean; cdecl;
begin
  Result := Atk1.atk_socket_is_occupied(@self);
end;

function TAtkStreamableContent.get_mime_type(i: gint): Pgchar; cdecl;
begin
  Result := Atk1.atk_streamable_content_get_mime_type(@self, i);
end;

function TAtkStreamableContent.get_n_mime_types: gint; cdecl;
begin
  Result := Atk1.atk_streamable_content_get_n_mime_types(@self);
end;

function TAtkStreamableContent.get_stream(mime_type: Pgchar): PGIOChannel; cdecl;
begin
  Result := Atk1.atk_streamable_content_get_stream(@self, mime_type);
end;

function TAtkStreamableContent.get_uri(mime_type: Pgchar): Pgchar; cdecl;
begin
  Result := Atk1.atk_streamable_content_get_uri(@self, mime_type);
end;

procedure TAtkValue.get_current_value(value: PGValue); cdecl;
begin
  Atk1.atk_value_get_current_value(@self, value);
end;

procedure TAtkValue.get_maximum_value(value: PGValue); cdecl;
begin
  Atk1.atk_value_get_maximum_value(@self, value);
end;

procedure TAtkValue.get_minimum_increment(value: PGValue); cdecl;
begin
  Atk1.atk_value_get_minimum_increment(@self, value);
end;

procedure TAtkValue.get_minimum_value(value: PGValue); cdecl;
begin
  Atk1.atk_value_get_minimum_value(@self, value);
end;

function TAtkValue.set_current_value(value: PGValue): gboolean; cdecl;
begin
  Result := Atk1.atk_value_set_current_value(@self, value);
end;

end.