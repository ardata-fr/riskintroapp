function isFunction(functionToCheck) {
  return functionToCheck && {}.toString.call(functionToCheck) === '[object Function]';
}
function parseFunction (str) {
  var fn_body_idx = str.indexOf('{'),
      fn_body = str.substring(fn_body_idx+1, str.lastIndexOf('}')),
      fn_declare = str.substring(0, fn_body_idx),
      fn_params = fn_declare.substring(fn_declare.indexOf('(')+1, fn_declare.lastIndexOf(')')),
      args = fn_params.split(',');

  args.push(fn_body);

  function Fn () {
    return Function.apply(this, args);
  }
  Fn.prototype = Function.prototype;
  return new Fn();
}

var customDragulaBinding = new Shiny.InputBinding();
$.extend(customDragulaBinding, {
  find: function(scope) {
    return $(scope).find(".shiny-input-dragula-custom");
  },
  onDoubleClick: function(evt) {
    var target = evt.currentTarget;
    if (target.classList.contains('dragula-target')) {
      var $block = $(evt.target).parents('.dragula-block');
      if ($block.length) {
        $block.remove();
        $(target).parents('.shiny-input-dragula-custom').trigger("change");
      }
    }
  },
  initialize: function(el) {
    var config = $(el).find('script[data-for="' + el.id + '"]');
    config = JSON.parse(config.html());

    var opts = config.options;

    if (!opts.hasOwnProperty("removeOnSpill")) {
      opts.removeOnSpill = false;
    }

    var containers = [];
    config.targets.forEach(function(id) {
      containers.push(document.querySelector("#" + id));
    });
    $(containers).on("dblclick", this.onDoubleClick);

    config.source.forEach(function(id) {
      containers.push(document.querySelector("#" + id));
    });

    if (config.replace) {
      opts.copy = function(element, source) {
        return source.classList.contains('dragula-source');
      };
      let userAcceptFunc = null;
      if (opts.accepts) {
        userAcceptFunc = parseFunction(opts.accepts.replaceAll("&amp;", "&"));
      }
      opts.accepts = function(element, target, source, sibling) {
        let accept = true;
        if (isFunction(userAcceptFunc)) {
          accept = userAcceptFunc(element, target, source, sibling);
        }
        if (accept && target && target != source && target.classList.contains('dragula-target')) {
          // don't accept if the item already exists
          const elId = element.firstElementChild.id;
          target.childNodes.forEach(function(x) {
            if (x.classList && !x.classList.contains('gu-transit') && x.firstElementChild && x.firstElementChild.id === elId) {
              accept = false;
            }
          });
        }
        return accept;
      };
    }

    el.drake = dragula(containers, opts);
    el.drake.on("dragend", function(element) {
      $(el).trigger("change");
    });

    if (config.replace) {
      el.drake.on("drop", function(element, target, source, sibling) {
        if (target && target.classList.contains('dragula-source')) {
          $(element).remove();
        } else if (target && target.classList.contains('dragula-target')) {
          let maxlength = target.getAttribute('data-max-length');
          if (maxlength) {
            maxlength = parseInt(maxlength, 10);
            const nodes = target.querySelectorAll(".dragula-block");
            const len = nodes.length;
            if (len > 0 && len > maxlength) {
              if (sibling) {
                target.removeChild(sibling);
              } else {
                target.removeChild(nodes[len - 2]);
              }
            }
          }
        }
      });
    }
  },
  getValue: function(el) {
    var config = $(el).find('script[data-for="' + el.id + '"]');
    config = JSON.parse(config.html());
    var values = {};
    values.source = [];
    values.target = {};
    var source = config.source;
    $("#" + source)
      .find("span.label-dragula")
      .each(function() {
        values.source.push($(this).data("value"));
      });

    var targets = config.targets;
    var targetsname = [];
    targets.forEach(function(element) {
      targetsname.push(element.replace(/.*target-/, ""));
    });

    for (i = 0; i < targetsname.length; i++) {
      values.target[targetsname[i]] = [];
      $("#" + targets[i])
        .find("span.label-dragula")
        .each(function() {
          values.target[targetsname[i]].push($(this).data("value"));
          if (values.target[targetsname[i]].length === 0) {
            values.target[targetsname[i]] = null;
          }
        });
    }

    if (values.source.length === 0) {
      values.source = null;
    }

    //console.log(values);
    return values;
  },
  getType: function() {
    return "custom.dragula";
  },
  setValue: function(el, value) {
    // Not implemented
  },
  subscribe: function(el, callback) {
    $(el).on("change.dragulaBinding", function(event) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".dragulaBinding");
    if (el.drake) {
      $(el.drake.containers).off("dblclick", this.onDoubleClick);
      el.drake.destroy();
      el.drake = null;
    }
  },
  receiveMessage: function(el, data) {
    var config = $(el).find('script[data-for="' + el.id + '"]');
    config = JSON.parse(config.html());

    if (data.hasOwnProperty("choices")) {
      config.targets.forEach(function(element) {
        $("#" + element)
          .children(".dragula-block")
          .remove();
      });

      config.source.forEach(function(element) {
        $("#" + element)
          .children(".dragula-block")
          .remove();
        $("#" + element).html(data.choices);
      });
    }
    if (data.hasOwnProperty("selected")) {
      config.targets.forEach(function(element) {
        $("#" + element)
          .children(".dragula-block")
          .remove();
        $("#" + element).html(data.selected[element]);
      });

    }
    $(el).trigger("change");
  },
  getRatePolicy: function() {
    return {
      policy: "debounce",
      delay: 250
    };
  }
});

Shiny.inputBindings.register(customDragulaBinding, "shiny.dragula.custom");

