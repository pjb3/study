/*!
 * Ext JS Library 3.0.0
 * Copyright(c) 2006-2009 Ext JS, LLC
 * licensing@extjs.com
 * http://www.extjs.com/license
 */
/**
 * @class Ext.FlashComponent
 * @extends Ext.BoxComponent
 * @constructor
 * @xtype flash
 */
Ext.FlashComponent = Ext.extend(Ext.BoxComponent, {
    /**
     * @cfg {String} flashVersion
     * Indicates the version the flash content was published for. Defaults to <tt>'9.0.45'</tt>.
     */
    flashVersion : '9.0.45',

    /**
     * @cfg {String} backgroundColor
     * The background color of the chart. Defaults to <tt>'#ffffff'</tt>.
     */
    backgroundColor: '#ffffff',

    /**
     * @cfg {String} wmode
     * The wmode of the flash object. This can be used to control layering. Defaults to <tt>'opaque'</tt>.
     */
    wmode: 'opaque',

    /**
     * @cfg {String} url
     * The URL of the chart to include. Defaults to <tt>undefined</tt>.
     */
    url: undefined,
    swfId : undefined,
    swfWidth: '100%',
    swfHeight: '100%',

    /**
     * @cfg {Boolean} expressInstall
     * True to prompt the user to install flash if not installed. Note that this uses
     * Ext.FlashComponent.EXPRESS_INSTALL_URL, which should be set to the local resource. Defaults to <tt>false</tt>.
     */
    expressInstall: false,

    initComponent : function(){
        Ext.FlashComponent.superclass.initComponent.call(this);

        this.addEvents('initialize');
    },

    onRender : function(){
        Ext.FlashComponent.superclass.onRender.apply(this, arguments);

        var params = {
            allowScriptAccess: 'always',
            bgcolor: this.backgroundColor,
            wmode: this.wmode
        }, vars = {
            allowedDomain: document.location.hostname,
            elementID: this.getId(),
            eventHandler: 'Ext.FlashEventProxy.onEvent'
        };

        new swfobject.embedSWF(this.url, this.id, this.swfWidth, this.swfHeight, this.flashVersion,
            this.expressInstall ? Ext.FlashComponent.EXPRESS_INSTALL_URL : undefined, vars, params);

        this.swf = Ext.getDom(this.id);
        this.el = Ext.get(this.swf);
    },

    getSwfId : function(){
        return this.swfId || (this.swfId = "extswf" + (++Ext.Component.AUTO_ID));
    },

    getId : function(){
        return this.id || (this.id = "extflashcmp" + (++Ext.Component.AUTO_ID));
    },

    onFlashEvent : function(e){
        switch(e.type){
            case "swfReady":
                this.initSwf();
                return;
            case "log":
                return;
        }
        e.component = this;
        this.fireEvent(e.type.toLowerCase().replace(/event$/, ''), e);
    },

    initSwf : function(){
        this.onSwfReady(!!this.isInitialized);
        this.isInitialized = true;
        this.fireEvent('initialize', this);
    },

    beforeDestroy: function(){
        if(this.rendered){
            swfobject.removeSWF(this.swf.id);
        }
        Ext.FlashComponent.superclass.beforeDestroy.call(this);
    },

    onSwfReady : Ext.emptyFn
});

/**
 * Sets the url for installing flash if it doesn't exist. This should be set to a local resource.
 * @static
 * @type String
 */
Ext.FlashComponent.EXPRESS_INSTALL_URL = 'http:/' + '/swfobject.googlecode.com/svn/trunk/swfobject/expressInstall.swf';

Ext.reg('flash', Ext.FlashComponent);