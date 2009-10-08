/*!
 * Ext JS Library 3.0.0
 * Copyright(c) 2006-2009 Ext JS, LLC
 * licensing@extjs.com
 * http://www.extjs.com/license
 */
Ext.lib.Event = function() {
    var loadComplete = false,
        listeners = [],
        unloadListeners = [],
        retryCount = 0,
        onAvailStack = [],
        _interval,
        locked = false,
        win = window,
        doc = document,
        
        // constants            
        POLL_RETRYS = 200,
        POLL_INTERVAL = 20,
        EL = 0,
        TYPE = 1,
        FN = 2,
        WFN = 3,
        OBJ = 3,
        ADJ_SCOPE = 4,   
        SCROLLLEFT = 'scrollLeft',
        SCROLLTOP = 'scrollTop',
        UNLOAD = 'unload',
        MOUSEOVER = 'mouseover',
        MOUSEOUT = 'mouseout',
        // private
        doAdd = function() {
            var ret;
            if (win.addEventListener) {
                ret = function(el, eventName, fn, capture) {
                    if (eventName == 'mouseenter') {
                        fn = fn.createInterceptor(checkRelatedTarget);
                        el.addEventListener(MOUSEOVER, fn, (capture));
                    } else if (eventName == 'mouseleave') {
                        fn = fn.createInterceptor(checkRelatedTarget);
                        el.addEventListener(MOUSEOUT, fn, (capture));
                    } else {
                        el.addEventListener(eventName, fn, (capture));
                    }
                    return fn;
                };
            } else if (win.attachEvent) {
                ret = function(el, eventName, fn, capture) {
                    el.attachEvent("on" + eventName, fn);
                    return fn;
                };
            } else {
                ret = function(){};
            }
            return ret;
        }(),    
        // private
        doRemove = function(){
            var ret;
            if (win.removeEventListener) {
                ret = function (el, eventName, fn, capture) {
                    if (eventName == 'mouseenter') {
                        eventName = MOUSEOVER;
                    } else if (eventName == 'mouseleave') {
                        eventName = MOUSEOUT;
                    }                        
                    el.removeEventListener(eventName, fn, (capture));
                };
            } else if (win.detachEvent) {
                ret = function (el, eventName, fn) {
                    el.detachEvent("on" + eventName, fn);
                };
            } else {
                ret = function(){};
            }
            return ret;
        }();        

    var isXUL = Ext.isGecko ? function(node){ 
        return Object.prototype.toString.call(node) == '[object XULElement]';
    } : function(){};
        
    var isTextNode = Ext.isGecko ? function(node){
        try{
            return node.nodeType == 3;
        }catch(e) {
            return false;
        }

    } : function(node){
        return node.nodeType == 3;
    };
        
    function checkRelatedTarget(e) {
        var related = pub.getRelatedTarget(e);
        return !(isXUL(related) || elContains(e.currentTarget,related));
    }

    function elContains(parent, child) {
       if(parent && parent.firstChild){  
         while(child) {
            if(child === parent) {
                return true;
            }
            try {
                child = child.parentNode;
            } catch(e) {
                // In FF if you mouseout an text input element
                // thats inside a div sometimes it randomly throws
                // Permission denied to get property HTMLDivElement.parentNode
                // See https://bugzilla.mozilla.org/show_bug.cgi?id=208427
                
                return false;
            }                
            if(child && (child.nodeType != 1)) {
                child = null;
            }
          }
        }
        return false;
    }

        
    // private  
    function _getCacheIndex(el, eventName, fn) {
        var index = -1;
        Ext.each(listeners, function (v,i) {
            if(v && v[FN] == fn && v[EL] == el && v[TYPE] == eventName) {
                index = i;
            }
        });
        return index;
    }
                    
    // private
    function _tryPreloadAttach() {
        var ret = false,                
            notAvail = [],
            element,
            tryAgain = !loadComplete || (retryCount > 0);                       
        
        if (!locked) {
            locked = true;
            
            Ext.each(onAvailStack, function (v,i,a){
                if(v && (element = doc.getElementById(v.id))){
                    if(!v.checkReady || loadComplete || element.nextSibling || (doc && doc.body)) {
                        element = v.override ? (v.override === true ? v.obj : v.override) : element;
                        v.fn.call(element, v.obj);
                        onAvailStack[i] = null;
                    } else {
                        notAvail.push(v);
                    }
                }   
            });

            retryCount = (notAvail.length === 0) ? 0 : retryCount - 1;

            if (tryAgain) { 
                startInterval();
            } else {
                clearInterval(_interval);
                _interval = null;
            }

            ret = !(locked = false);
        }
        return ret;
    }
    
    // private              
    function startInterval() {            
        if(!_interval){                    
            var callback = function() {
                _tryPreloadAttach();
            };
            _interval = setInterval(callback, POLL_INTERVAL);
        }
    }
    
    // private 
    function getScroll() {
        var dd = doc.documentElement, 
            db = doc.body;
        if(dd && (dd[SCROLLTOP] || dd[SCROLLLEFT])){
            return [dd[SCROLLLEFT], dd[SCROLLTOP]];
        }else if(db){
            return [db[SCROLLLEFT], db[SCROLLTOP]];
        }else{
            return [0, 0];
        }
    }
        
    // private
    function getPageCoord (ev, xy) {
        ev = ev.browserEvent || ev;
        var coord  = ev['page' + xy];
        if (!coord && coord !== 0) {
            coord = ev['client' + xy] || 0;

            if (Ext.isIE) {
                coord += getScroll()[xy == "X" ? 0 : 1];
            }
        }

        return coord;
    }

    var pub =  {
        onAvailable : function(p_id, p_fn, p_obj, p_override) {             
            onAvailStack.push({ 
                id:         p_id,
                fn:         p_fn,
                obj:        p_obj,
                override:   p_override,
                checkReady: false });

            retryCount = POLL_RETRYS;
            startInterval();
        },


        addListener: function(el, eventName, fn) {
            var ret;                
            el = Ext.getDom(el);                
            if (el && fn) {
                if (UNLOAD == eventName) {
                    ret = !!(unloadListeners[unloadListeners.length] = [el, eventName, fn]);                    
                } else {
                    listeners.push([el, eventName, fn, ret = doAdd(el, eventName, fn, false)]);
                }
            }
            return !!ret;
        },

        removeListener: function(el, eventName, fn) {
            var ret = false,
                index, 
                cacheItem;

            el = Ext.getDom(el);

            if(!fn) {                   
                ret = this.purgeElement(el, false, eventName);
            } else if (UNLOAD == eventName) {   
                Ext.each(unloadListeners, function(v, i, a) {
                    if( v && v[0] == el && v[1] == eventName && v[2] == fn) {
                        unloadListeners.splice(i, 1);
                        ret = true;
                    }
                });
            } else {    
                index = arguments[3] || _getCacheIndex(el, eventName, fn);
                cacheItem = listeners[index];
                
                if (el && cacheItem) {
                    doRemove(el, eventName, cacheItem[WFN], false);     
                    cacheItem[WFN] = cacheItem[FN] = null;                       
                    listeners.splice(index, 1);     
                    ret = true;
                }
            }
            return ret;
        },

        getTarget : function(ev) {
            ev = ev.browserEvent || ev;                
            return this.resolveTextNode(ev.target || ev.srcElement);
        },

        resolveTextNode : function(node) {
            return node && !isXUL(node) && isTextNode(node) ? node.parentNode : node;
        },

        getRelatedTarget : function(ev) {
            ev = ev.browserEvent || ev;
            return this.resolveTextNode(ev.relatedTarget || 
                    (ev.type == MOUSEOUT ? ev.toElement :
                     ev.type == MOUSEOVER ? ev.fromElement : null));
        },
        
        getPageX : function(ev) {
            return getPageCoord(ev, "X");
        },

        getPageY : function(ev) {
            return getPageCoord(ev, "Y");
        },


        getXY : function(ev) {                             
            return [this.getPageX(ev), this.getPageY(ev)];
        },

// Is this useful?  Removing to save space unless use case exists.
//             getTime: function(ev) {
//                 ev = ev.browserEvent || ev;
//                 if (!ev.time) {
//                     var t = new Date().getTime();
//                     try {
//                         ev.time = t;
//                     } catch(ex) {
//                         return t;
//                     }
//                 }

//                 return ev.time;
//             },

        stopEvent : function(ev) {                            
            this.stopPropagation(ev);
            this.preventDefault(ev);
        },

        stopPropagation : function(ev) {
            ev = ev.browserEvent || ev;
            if (ev.stopPropagation) {
                ev.stopPropagation();
            } else {
                ev.cancelBubble = true;
            }
        },

        preventDefault : function(ev) {
            ev = ev.browserEvent || ev;
            if (ev.preventDefault) {
                ev.preventDefault();
            } else {
                ev.returnValue = false;
            }
        },
        
        getEvent : function(e) {
            e = e || win.event;
            if (!e) {
                var c = this.getEvent.caller;
                while (c) {
                    e = c.arguments[0];
                    if (e && Event == e.constructor) {
                        break;
                    }
                    c = c.caller;
                }
            }
            return e;
        },

        getCharCode : function(ev) {
            ev = ev.browserEvent || ev;
            return ev.charCode || ev.keyCode || 0;
        },

        //clearCache: function() {},

        _load : function(e) {
            loadComplete = true;
            var EU = Ext.lib.Event;    
            if (Ext.isIE && e !== true) {
        // IE8 complains that _load is null or not an object
        // so lets remove self via arguments.callee
                doRemove(win, "load", arguments.callee);
            }
        },            
        
        purgeElement : function(el, recurse, eventName) {
            var me = this;
            Ext.each( me.getListeners(el, eventName), function(v){
                if(v){
                    me.removeListener(el, v.type, v.fn);
                }
            });

            if (recurse && el && el.childNodes) {
                Ext.each(el.childNodes, function(v){
                    me.purgeElement(v, recurse, eventName);
                });
            }
        },

        getListeners : function(el, eventName) {
            var me = this,
                results = [], 
                searchLists;

            if (eventName){  
                searchLists = eventName == UNLOAD ? unloadListeners : listeners;
            }else{
                searchLists = listeners.concat(unloadListeners);
            }

            Ext.each(searchLists, function(v, i){
                if (v && v[EL] == el && (!eventName || eventName == v[TYPE])) {
                    results.push({
                                type:   v[TYPE],
                                fn:     v[FN],
                                obj:    v[OBJ],
                                adjust: v[ADJ_SCOPE],
                                index:  i
                            });
                }   
            });                

            return results.length ? results : null;
        },

        _unload : function(e) {
             var EU = Ext.lib.Event, 
                i, 
                j, 
                l, 
                len, 
                index,
                scope;
                

            Ext.each(unloadListeners, function(v) {
                if (v) {
                    try{
                        scope =  v[ADJ_SCOPE] ? (v[ADJ_SCOPE] === true ? v[OBJ] : v[ADJ_SCOPE]) :  win; 
                        v[FN].call(scope, EU.getEvent(e), v[OBJ]);
                    }catch(ex){}
                }   
            });     

            unloadListeners = null;

            if(listeners && (j = listeners.length)){                    
                while(j){                        
                    if((l = listeners[index = --j])){
                        EU.removeListener(l[EL], l[TYPE], l[FN], index);
                    }                        
                }
                //EU.clearCache();
            }

            doRemove(win, UNLOAD, EU._unload);
        }            
    };        
    
    // Initialize stuff.
    pub.on = pub.addListener;
    pub.un = pub.removeListener;
    if (doc && doc.body) {
        pub._load(true);
    } else {
        doAdd(win, "load", pub._load);
    }
    doAdd(win, UNLOAD, pub._unload);    
    _tryPreloadAttach();
    
    return pub;
}();