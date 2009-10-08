/*!
 * Ext JS Library 3.0.0
 * Copyright(c) 2006-2009 Ext JS, LLC
 * licensing@extjs.com
 * http://www.extjs.com/license
 */
/**
 * @class Ext.layout.MenuLayout
 * @extends Ext.layout.ContainerLayout
 * <p>Layout manager used by {@link Ext.menu.Menu}. Generally this class should not need to be used directly.</p>
 */
 Ext.layout.MenuLayout = Ext.extend(Ext.layout.ContainerLayout, {
    monitorResize: true,

    setContainer : function(ct){
        this.monitorResize = !ct.floating;
        Ext.layout.MenuLayout.superclass.setContainer.call(this, ct);
    },

    renderItem : function(c, position, target){
        if (!this.itemTpl) {
            this.itemTpl = Ext.layout.MenuLayout.prototype.itemTpl = new Ext.XTemplate(
                '<li id="{itemId}" class="{itemCls}">',
                    '<tpl if="needsIcon">',
                        '<img src="{icon}" class="{iconCls}"/>',
                    '</tpl>',
                '</li>'
            );
        }

        if(c && !c.rendered){
            if(Ext.isNumber(position)){
                position = target.dom.childNodes[position];
            }
            var a = this.getItemArgs(c);

//          The Component's positionEl is the <li> it is rendered into
            c.render(c.positionEl = position ?
                this.itemTpl.insertBefore(position, a, true) :
                this.itemTpl.append(target, a, true));

//          Link the containing <li> to the item.
            c.positionEl.menuItemId = c.itemId || c.id;

//          If rendering a regular Component, and it needs an icon,
//          move the Component rightwards.
            if (!a.isMenuItem && a.needsIcon) {
                c.positionEl.addClass('x-menu-list-item-indent');
            }
        }else if(c && !this.isValidParent(c, target)){
            if(Ext.isNumber(position)){
                position = target.dom.childNodes[position];
            }
            target.dom.insertBefore(c.getActionEl().dom, position || null);
        }
    },

    getItemArgs: function(c) {
        var isMenuItem = c instanceof Ext.menu.Item;
        return {
            isMenuItem: isMenuItem,
            needsIcon: !isMenuItem && (c.icon || c.iconCls),
            icon: c.icon || Ext.BLANK_IMAGE_URL,
            iconCls: 'x-menu-item-icon ' + (c.iconCls || ''),
            itemId: 'x-menu-el-' + c.id,
            itemCls: 'x-menu-list-item ' + (this.extraCls || '')
        };
    },

//  Valid if the Component is in a <li> which is part of our target <ul>
    isValidParent: function(c, target) {
        return c.el.up('li.x-menu-list-item', 5).dom.parentNode === (target.dom || target);
    },

    onLayout : function(ct, target){
        this.renderAll(ct, target);
        this.doAutoSize();
    },

    doAutoSize : function(){
        var ct = this.container, w = ct.width;
        if(ct.floating){
            if(w){
                ct.setWidth(w);
            }else if(Ext.isIE){
                ct.setWidth(Ext.isStrict && (Ext.isIE7 || Ext.isIE8) ? 'auto' : ct.minWidth);
                var el = ct.getEl(), t = el.dom.offsetWidth; // force recalc
                ct.setWidth(ct.getLayoutTarget().getWidth() + el.getFrameWidth('lr'));
            }
        }
    }
});
Ext.Container.LAYOUTS['menu'] = Ext.layout.MenuLayout;

/**
 * @class Ext.menu.Menu
 * @extends Ext.Container
 * <p>A menu object.  This is the container to which you may add menu items.  Menu can also serve as a base class
 * when you want a specialized menu based off of another component (like {@link Ext.menu.DateMenu} for example).</p>
 * <p>Menus may contain either {@link Ext.menu.Item menu items}, or general {@link Ext.Component Component}s.</p>
 * <p>To make a contained general {@link Ext.Component Component} line up with other {@link Ext.menu.Item menu items}
 * specify <tt>iconCls: 'no-icon'</tt>.  This reserves a space for an icon, and indents the Component in line
 * with the other menu items.  See {@link Ext.form.ComboBox}.{@link Ext.form.ComboBox#getListParent getListParent}
 * for an example.</p>
 * <p>By default, Menus are absolutely positioned, floating Components. By configuring a Menu with
 * <b><tt>{@link #floating}:false</tt></b>, a Menu may be used as child of a Container.</p>
 *
 * @xtype menu
 */
Ext.menu.Menu = Ext.extend(Ext.Container, {
    /**
     * @cfg {Object} defaults
     * A config object that will be applied to all items added to this container either via the {@link #items}
     * config or via the {@link #add} method.  The defaults config can contain any number of
     * name/value property pairs to be added to each item, and should be valid for the types of items
     * being added to the menu.
     */
    /**
     * @cfg {Mixed} items
     * An array of items to be added to this menu. Menus may contain either {@link Ext.menu.Item menu items},
     * or general {@link Ext.Component Component}s.
     */
    /**
     * @cfg {Number} minWidth The minimum width of the menu in pixels (defaults to 120)
     */
    minWidth : 120,
    /**
     * @cfg {Boolean/String} shadow True or "sides" for the default effect, "frame" for 4-way shadow, and "drop"
     * for bottom-right shadow (defaults to "sides")
     */
    shadow : "sides",
    /**
     * @cfg {String} subMenuAlign The {@link Ext.Element#alignTo} anchor position value to use for submenus of
     * this menu (defaults to "tl-tr?")
     */
    subMenuAlign : "tl-tr?",
    /**
     * @cfg {String} defaultAlign The default {@link Ext.Element#alignTo} anchor position value for this menu
     * relative to its element of origin (defaults to "tl-bl?")
     */
    defaultAlign : "tl-bl?",
    /**
     * @cfg {Boolean} allowOtherMenus True to allow multiple menus to be displayed at the same time (defaults to false)
     */
    allowOtherMenus : false,
    /**
     * @cfg {Boolean} ignoreParentClicks True to ignore clicks on any item in this menu that is a parent item (displays
     * a submenu) so that the submenu is not dismissed when clicking the parent item (defaults to false).
     */
    ignoreParentClicks : false,
    /**
     * @cfg {Boolean} enableScrolling True to allow the menu container to have scroller controls if the menu is too long (defaults to true).
     */
    enableScrolling: true,
    /**
     * @cfg {Number} maxHeight The maximum height of the menu. Only applies when enableScrolling is set to True (defaults to null).
     */
    maxHeight: null,
    /**
     * @cfg {Number} scrollIncrement The amount to scroll the menu. Only applies when enableScrolling is set to True (defaults to 24).
     */
    scrollIncrement: 24,
    /**
     * @cfg {Boolean} showSeparator True to show the icon separator. (defaults to true).
     */
    showSeparator: true,
    /**
     * @cfg {Array} defaultOffsets An array specifying the [x, y] offset in pixels by which to
     * change the default Menu popup position after aligning according to the {@link #defaultAlign}
     * configuration. Defaults to <tt>[0, 0]</tt>.
     */
    defaultOffsets : [0, 0],


    /**
     * @cfg {Boolean} floating
     * May be specified as false to create a Menu which may be used as a child item of another Container
     * instead of a free-floating {@link Ext.Layer Layer}. (defaults to true).
     */
    floating: true,         // Render as a Layer by default

    // private
    hidden: true,
    layout: 'menu',
    hideMode: 'offsets',    // Important for laying out Components
    scrollerHeight: 8,
    autoLayout: true,       // Provided for backwards compat
    defaultType: 'menuitem',

    initComponent: function(){
        if(Ext.isArray(this.initialConfig)){
            Ext.apply(this, {items:this.initialConfig});
        }
        this.addEvents(
            /**
             * @event click
             * Fires when this menu is clicked (or when the enter key is pressed while it is active)
             * @param {Ext.menu.Menu} this
            * @param {Ext.menu.Item} menuItem The menu item that was clicked
             * @param {Ext.EventObject} e
             */
            'click',
            /**
             * @event mouseover
             * Fires when the mouse is hovering over this menu
             * @param {Ext.menu.Menu} this
             * @param {Ext.EventObject} e
             * @param {Ext.menu.Item} menuItem The menu item that was clicked
             */
            'mouseover',
            /**
             * @event mouseout
             * Fires when the mouse exits this menu
             * @param {Ext.menu.Menu} this
             * @param {Ext.EventObject} e
             * @param {Ext.menu.Item} menuItem The menu item that was clicked
             */
            'mouseout',
            /**
             * @event itemclick
             * Fires when a menu item contained in this menu is clicked
             * @param {Ext.menu.BaseItem} baseItem The BaseItem that was clicked
             * @param {Ext.EventObject} e
             */
            'itemclick'
        );
        Ext.menu.MenuMgr.register(this);
        if(this.floating){
            Ext.EventManager.onWindowResize(this.hide, this);
        }else{
            if(this.initialConfig.hidden !== false){
                this.hidden = false;
            }
            this.internalDefaults = {hideOnClick: false};
        }
        Ext.menu.Menu.superclass.initComponent.call(this);
        if(this.autoLayout){
            this.on({
                add: this.doLayout,
                remove: this.doLayout,
                scope: this
            });
        }
    },

    //private
    getLayoutTarget : function() {
        return this.ul;
    },

    // private
    onRender : function(ct, position){
        if(!ct){
            ct = Ext.getBody();
        }

        var dh = {
            id: this.getId(),
            cls: 'x-menu ' + ((this.floating) ? 'x-menu-floating x-layer ' : '') + (this.cls || '') + (this.plain ? ' x-menu-plain' : '') + (this.showSeparator ? '' : ' x-menu-nosep'),
            style: this.style,
            cn: [
                {tag: 'a', cls: 'x-menu-focus', href: '#', onclick: 'return false;', tabIndex: '-1'},
                {tag: 'ul', cls: 'x-menu-list'}
            ]
        };
        if(this.floating){
            this.el = new Ext.Layer({
                shadow: this.shadow,
                dh: dh,
                constrain: false,
                parentEl: ct,
                zindex:15000
            });
        }else{
            this.el = ct.createChild(dh);
        }
        Ext.menu.Menu.superclass.onRender.call(this, ct, position);

        if(!this.keyNav){
            this.keyNav = new Ext.menu.MenuNav(this);
        }
        // generic focus element
        this.focusEl = this.el.child('a.x-menu-focus');
        this.ul = this.el.child('ul.x-menu-list');
        this.mon(this.ul, {
            scope: this,
            click: this.onClick,
            mouseover: this.onMouseOver,
            mouseout: this.onMouseOut
        });
        if(this.enableScrolling){
            this.mon(this.el, {
                scope: this,
                delegate: '.x-menu-scroller',
                click: this.onScroll,
                mouseover: this.deactivateActive
            });
        }
    },

    // private
    findTargetItem : function(e){
        var t = e.getTarget(".x-menu-list-item", this.ul, true);
        if(t && t.menuItemId){
            return this.items.get(t.menuItemId);
        }
    },

    // private
    onClick : function(e){
        var t = this.findTargetItem(e);
        if(t){
            if(t.isFormField){
                this.setActiveItem(t);
            }else{
                if(t.menu && this.ignoreParentClicks){
                    t.expandMenu();
                    e.preventDefault();
                }else if(t.onClick){
                    t.onClick(e);
                    this.fireEvent("click", this, t, e);
                }
            }
        }
    },

    // private
    setActiveItem : function(item, autoExpand){
        if(item != this.activeItem){
            this.deactivateActive();
            if((this.activeItem = item).isFormField){
                item.focus();
            }else{
                item.activate(autoExpand);
            }
        }else if(autoExpand){
            item.expandMenu();
        }
    },

    deactivateActive: function(){
        var a = this.activeItem;
        if(a){
            if(a.isFormField){
                //Fields cannot deactivate, but Combos must collapse
                if(a.collapse){
                    a.collapse();
                }
            }else{
                a.deactivate();
            }
            delete this.activeItem;
        }
    },

    // private
    tryActivate : function(start, step){
        var items = this.items;
        for(var i = start, len = items.length; i >= 0 && i < len; i+= step){
            var item = items.get(i);
            if(!item.disabled && (item.canActivate || item.isFormField)){
                this.setActiveItem(item, false);
                return item;
            }
        }
        return false;
    },

    // private
    onMouseOver : function(e){
        var t = this.findTargetItem(e);
        if(t){
            if(t.canActivate && !t.disabled){
                this.setActiveItem(t, true);
            }
        }
        this.over = true;
        this.fireEvent("mouseover", this, e, t);
    },

    // private
    onMouseOut : function(e){
        var t = this.findTargetItem(e);
        if(t){
            if(t == this.activeItem && t.shouldDeactivate && t.shouldDeactivate(e)){
                this.activeItem.deactivate();
                delete this.activeItem;
            }
        }
        this.over = false;
        this.fireEvent("mouseout", this, e, t);
    },

    // private
    onScroll: function(e, t){
        if(e){
            e.stopEvent();
        }
        var ul = this.ul.dom, top = Ext.fly(t).is('.x-menu-scroller-top');
        ul.scrollTop += this.scrollIncrement * (top ? -1 : 1);
        if(top ? ul.scrollTop <= 0 : ul.scrollTop + this.activeMax >= ul.scrollHeight){
           this.onScrollerOut(null, t);
        }
    },

    // private
    onScrollerIn: function(e, t){
        var ul = this.ul.dom, top = Ext.fly(t).is('.x-menu-scroller-top');
        if(top ? ul.scrollTop > 0 : ul.scrollTop + this.activeMax < ul.scrollHeight){
            Ext.fly(t).addClass(['x-menu-item-active', 'x-menu-scroller-active']);
        }
    },

    // private
    onScrollerOut: function(e, t){
        Ext.fly(t).removeClass(['x-menu-item-active', 'x-menu-scroller-active']);
    },

    /**
     * Displays this menu relative to another element
     * @param {Mixed} element The element to align to
     * @param {String} position (optional) The {@link Ext.Element#alignTo} anchor position to use in aligning to
     * the element (defaults to this.defaultAlign)
     * @param {Ext.menu.Menu} parentMenu (optional) This menu's parent menu, if applicable (defaults to undefined)
     */
    show : function(el, pos, parentMenu){
        if(this.floating){
            this.parentMenu = parentMenu;
            if(!this.el){
                this.render();
                this.doLayout(false, true);
            }
            if(this.fireEvent('beforeshow', this) !== false){
                this.showAt(this.el.getAlignToXY(el, pos || this.defaultAlign, this.defaultOffsets), parentMenu, false);
            }
        }else{
            Ext.menu.Menu.superclass.show.call(this);
        }
    },

    /**
     * Displays this menu at a specific xy position
     * @param {Array} xyPosition Contains X & Y [x, y] values for the position at which to show the menu (coordinates are page-based)
     * @param {Ext.menu.Menu} parentMenu (optional) This menu's parent menu, if applicable (defaults to undefined)
     */
    showAt : function(xy, parentMenu, /* private: */_e){
        this.parentMenu = parentMenu;
        if(!this.el){
            this.render();
        }
        this.el.setXY(xy);
        if(this.enableScrolling){
            this.constrainScroll(xy[1]);
        }
        this.el.show();
        Ext.menu.Menu.superclass.onShow.call(this);
        if(Ext.isIE){
            this.layout.doAutoSize();
            if(!Ext.isIE8){
                this.el.repaint();
            }
        }
        this.hidden = false;
        this.focus();
        this.fireEvent("show", this);
    },

    constrainScroll: function(y){
        var max, full = this.ul.setHeight('auto').getHeight();
        if(this.floating){
            max = this.maxHeight ? this.maxHeight : Ext.fly(this.el.dom.parentNode).getViewSize().height - y;
        }else{
            max = this.getHeight();
        }
        if(full > max && max > 0){
            this.activeMax = max - this.scrollerHeight * 2 - this.el.getFrameWidth('tb') - Ext.num(this.el.shadowOffset, 0);
            this.ul.setHeight(this.activeMax);
            this.createScrollers();
            this.el.select('.x-menu-scroller').setDisplayed('');
        }else{
            this.ul.setHeight(full);
            this.el.select('.x-menu-scroller').setDisplayed('none');
        }
        this.ul.dom.scrollTop = 0;
    },

    createScrollers: function(){
        if(!this.scroller){
            this.scroller = {
                pos: 0,
                top: this.el.insertFirst({
                    tag: 'div',
                    cls: 'x-menu-scroller x-menu-scroller-top',
                    html: '&#160;'
                }),
                bottom: this.el.createChild({
                    tag: 'div',
                    cls: 'x-menu-scroller x-menu-scroller-bottom',
                    html: '&#160;'
                })
            };
            this.scroller.top.hover(this.onScrollerIn, this.onScrollerOut, this);
            this.scroller.topRepeater = new Ext.util.ClickRepeater(this.scroller.top, {
                listeners: {
                    click: this.onScroll.createDelegate(this, [null, this.scroller.top], false)
                }
            });
            this.scroller.bottom.hover(this.onScrollerIn, this.onScrollerOut, this);
            this.scroller.bottomRepeater = new Ext.util.ClickRepeater(this.scroller.bottom, {
                listeners: {
                    click: this.onScroll.createDelegate(this, [null, this.scroller.bottom], false)
                }
            });
        }
    },

    onLayout: function(){
        if(this.isVisible()){
            if(this.enableScrolling){
                this.constrainScroll(this.el.getTop());
            }
            if(this.floating){
                this.el.sync();
            }
        }
    },

    focus : function(){
        if(!this.hidden){
            this.doFocus.defer(50, this);
        }
    },

    doFocus : function(){
        if(!this.hidden){
            this.focusEl.focus();
        }
    },

    /**
     * Hides this menu and optionally all parent menus
     * @param {Boolean} deep (optional) True to hide all parent menus recursively, if any (defaults to false)
     */
    hide : function(deep){
        this.deepHide = deep;
        Ext.menu.Menu.superclass.hide.call(this);
        delete this.deepHide;
    },

    // private
    onHide: function(){
        Ext.menu.Menu.superclass.onHide.call(this);
        this.deactivateActive();
        if(this.el && this.floating){
            this.el.hide();
        }
        if(this.deepHide === true && this.parentMenu){
            this.parentMenu.hide(true);
        }
    },

    // private
    lookupComponent: function(c){
         if(Ext.isString(c)){
            c = (c == 'separator' || c == '-') ? new Ext.menu.Separator() : new Ext.menu.TextItem(c);
             this.applyDefaults(c);
         }else{
            if(Ext.isObject(c)){
                c = this.getMenuItem(c);
            }else if(c.tagName || c.el){ // element. Wrap it.
                c = new Ext.BoxComponent({
                    el: c
                });
            }
         }
         return c;
    },

    applyDefaults : function(c){
        if(!Ext.isString(c)){
            c = Ext.menu.Menu.superclass.applyDefaults.call(this, c);
            var d = this.internalDefaults;
            if(d){
                if(c.events){
                    Ext.applyIf(c.initialConfig, d);
                    Ext.apply(c, d);
                }else{
                    Ext.applyIf(c, d);
                }
            }
        }
        return c;
    },

    // private
    getMenuItem: function(config){
       if(!config.isXType){
            if(!config.xtype && Ext.isBoolean(config.checked)){
                return new Ext.menu.CheckItem(config)
            }
            return Ext.create(config, this.defaultType);
        }
        return config;
    },

    /**
     * Adds a separator bar to the menu
     * @return {Ext.menu.Item} The menu item that was added
     */
    addSeparator : function(){
        return this.add(new Ext.menu.Separator());
    },

    /**
     * Adds an {@link Ext.Element} object to the menu
     * @param {Mixed} el The element or DOM node to add, or its id
     * @return {Ext.menu.Item} The menu item that was added
     */
    addElement : function(el){
        return this.add(new Ext.menu.BaseItem(el));
    },

    /**
     * Adds an existing object based on {@link Ext.menu.BaseItem} to the menu
     * @param {Ext.menu.Item} item The menu item to add
     * @return {Ext.menu.Item} The menu item that was added
     */
    addItem : function(item){
        return this.add(item);
    },

    /**
     * Creates a new {@link Ext.menu.Item} based an the supplied config object and adds it to the menu
     * @param {Object} config A MenuItem config object
     * @return {Ext.menu.Item} The menu item that was added
     */
    addMenuItem : function(config){
        return this.add(this.getMenuItem(config));
    },

    /**
     * Creates a new {@link Ext.menu.TextItem} with the supplied text and adds it to the menu
     * @param {String} text The text to display in the menu item
     * @return {Ext.menu.Item} The menu item that was added
     */
    addText : function(text){
        return this.add(new Ext.menu.TextItem(text));
    },

    //private
    onDestroy : function(){
        Ext.menu.Menu.superclass.onDestroy.call(this);
        Ext.menu.MenuMgr.unregister(this);
        Ext.EventManager.removeResizeListener(this.hide, this);
        if(this.keyNav) {
            this.keyNav.disable();
        }
        var s = this.scroller;
        if(s){
            Ext.destroy(s.topRepeater, s.bottomRepeater, s.top, s.bottom);
        }
    }
});

Ext.reg('menu', Ext.menu.Menu);

// MenuNav is a private utility class used internally by the Menu
Ext.menu.MenuNav = Ext.extend(Ext.KeyNav, function(){
    function up(e, m){
        if(!m.tryActivate(m.items.indexOf(m.activeItem)-1, -1)){
            m.tryActivate(m.items.length-1, -1);
        }
    }
    function down(e, m){
        if(!m.tryActivate(m.items.indexOf(m.activeItem)+1, 1)){
            m.tryActivate(0, 1);
        }
    }
    return {
        constructor: function(menu){
            Ext.menu.MenuNav.superclass.constructor.call(this, menu.el);
            this.scope = this.menu = menu;
        },

        doRelay : function(e, h){
            var k = e.getKey();
//          Keystrokes within a form Field (e.g.: down in a Combo) do not navigate. Allow only TAB
            if (this.menu.activeItem && this.menu.activeItem.isFormField && k != e.TAB) {
                return false;
            }
            if(!this.menu.activeItem && e.isNavKeyPress() && k != e.SPACE && k != e.RETURN){
                this.menu.tryActivate(0, 1);
                return false;
            }
            return h.call(this.scope || this, e, this.menu);
        },

        tab: function(e, m) {
            e.stopEvent();
            if (e.shiftKey) {
                up(e, m);
            } else {
                down(e, m);
            }
        },

        up : up,

        down : down,

        right : function(e, m){
            if(m.activeItem){
                m.activeItem.expandMenu(true);
            }
        },

        left : function(e, m){
            m.hide();
            if(m.parentMenu && m.parentMenu.activeItem){
                m.parentMenu.activeItem.activate();
            }
        },

        enter : function(e, m){
            if(m.activeItem){
                e.stopPropagation();
                m.activeItem.onClick(e);
                m.fireEvent("click", this, m.activeItem);
                return true;
            }
        }
    };
}());