(function() {
  var HomeView, MainController, Rule, RulesList, RulesListItemView, RulesListView, RulesTree, RulesTreeItemView, RulesTreeView;
  var __hasProp = Object.prototype.hasOwnProperty, __extends = function(child, parent) {
    for (var key in parent) { if (__hasProp.call(parent, key)) child[key] = parent[key]; }
    function ctor() { this.constructor = child; }
    ctor.prototype = parent.prototype;
    child.prototype = new ctor;
    child.__super__ = parent.prototype;
    return child;
  }, __bind = function(fn, me){ return function(){ return fn.apply(me, arguments); }; };
  window.app = {};
  app.controllers = {};
  app.models = {};
  app.collections = {};
  app.views = {};
  app.styles = {};
  app.templates = {};
  $(document).ready(function() {
    app.initialize = function() {
      app.collections.rulesList = new RulesList();
      app.collections.rulesTree = new RulesTree();
      app.controllers.main = new MainController();
      app.views.home = new HomeView();
      app.views.rulesList = new RulesListView();
      app.views.rulesTree = new RulesTreeView();
      if (Backbone.history.getFragment() === '') {
        return Backbone.history.saveLocation("home");
      }
    };
    app.initialize();
    return Backbone.history.start();
  });
  Rule = (function() {
    function Rule() {
      Rule.__super__.constructor.apply(this, arguments);
    }
    __extends(Rule, Backbone.Model);
    Rule.prototype.clear = function() {
      this.destroy();
      return this.view.remove();
    };
    return Rule;
  })();
  RulesList = (function() {
    function RulesList() {
      RulesList.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesList, Backbone.Collection);
    RulesList.prototype.model = Rule;
    RulesList.prototype.url = function() {
      return '/rules/stored';
    };
    return RulesList;
  })();
  RulesTree = (function() {
    function RulesTree() {
      RulesTree.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesTree, Backbone.Collection);
    RulesTree.prototype.model = Rule;
    RulesTree.prototype.url = function() {
      return '/rules/inuse';
    };
    return RulesTree;
  })();
  MainController = (function() {
    __extends(MainController, Backbone.Controller);
    MainController.prototype.routes = {
      "home": "home"
    };
    function MainController() {
      MainController.__super__.constructor.apply(this, arguments);
    }
    MainController.prototype.home = function() {
      return $('body').html(app.views.home.render().el);
    };
    return MainController;
  })();
  HomeView = (function() {
    function HomeView() {
      HomeView.__super__.constructor.apply(this, arguments);
    }
    __extends(HomeView, Backbone.View);
    HomeView.prototype.id = 'home-view';
    HomeView.prototype.render = function() {
      $(this.el).html(app.templates.home());
      $(this.el).find('#rules-tree-div').append(app.views.rulesTree.render().el);
      $(this.el).find('#rules-list-div').append(app.views.rulesList.render().el);
      return this;
    };
    return HomeView;
  })();
  RulesListItemView = (function() {
    function RulesListItemView() {
      this.render = __bind(this.render, this);;      RulesListItemView.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesListItemView, Backbone.View);
    RulesListItemView.prototype.tagName = 'li';
    RulesListItemView.prototype.initialize = function() {
      return this.model.view = this;
    };
    RulesListItemView.prototype.render = function() {
      $(this.el).html(app.templates.rulesListItem({
        content: this.model.toJSON()
      }));
      return this;
    };
    RulesListItemView.prototype.remove = function() {
      return $(this.el).remove();
    };
    RulesListItemView.prototype.clear = function() {
      return this.model.clear();
    };
    return RulesListItemView;
  })();
  RulesListView = (function() {
    function RulesListView() {
      this.renderList = __bind(this.renderList, this);;
      this.addAll = __bind(this.addAll, this);;
      this.addOne = __bind(this.addOne, this);;      RulesListView.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesListView, Backbone.View);
    RulesListView.prototype.id = 'rules-list-view';
    RulesListView.prototype.tagName = 'ul';
    RulesListView.prototype.initialize = function() {
      _.bindAll(this, 'addOne', 'addAll', 'render');
      console.log(app.collections.rulesList);
      app.collections.rulesList.bind('add', this.addOne);
      app.collections.rulesList.bind('refresh', this.addAll);
      app.collections.rulesList.bind('all', this.renderList);
      return app.collections.rulesList.fetch();
    };
    RulesListView.prototype.addOne = function(rule) {
      var view;
      view = new RulesListItemView({
        model: rule
      });
      return $(this.el).append(view.render().el);
    };
    RulesListView.prototype.addAll = function() {
      return app.collections.rulesList.each(this.addOne);
    };
    RulesListView.prototype.renderList = function() {
      app.views.rulesList.render();
      return $('.draggable').draggable({
        revert: true,
        revertDuration: 100
      });
    };
    return RulesListView;
  })();
  RulesTreeItemView = (function() {
    function RulesTreeItemView() {
      this.render = __bind(this.render, this);;      RulesTreeItemView.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesTreeItemView, Backbone.View);
    RulesTreeItemView.prototype.tagName = "li";
    RulesTreeItemView.prototype.initialize = function() {
      return this.model.view = this;
    };
    RulesTreeItemView.prototype.render = function() {
      $(this.el).html(app.templates.rulesTreeItem({
        content: this.model.toJSON(),
        isTerm: this.options.isTerm
      }));
      $(this.el).droppable({
        hoverClass: 'dropHover',
        drop: function(event, ui) {
          var ruleSpan;
          ruleSpan = ui.draggable.find(".rule-text")[0];
          return $(this).find("input[type='text']").val(ruleSpan.innerHTML);
        }
      });
      return this;
    };
    RulesTreeItemView.prototype.remove = function() {
      return $(this.el).remove();
    };
    RulesTreeItemView.prototype.clear = function() {
      return this.model.clear();
    };
    return RulesTreeItemView;
  })();
  RulesTreeView = (function() {
    function RulesTreeView() {
      this.renderList = __bind(this.renderList, this);;
      this.addAll = __bind(this.addAll, this);;
      this.addOne = __bind(this.addOne, this);;      RulesTreeView.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesTreeView, Backbone.View);
    RulesTreeView.prototype.id = 'rules-tree-view';
    RulesTreeView.prototype.tagName = 'ul';
    RulesTreeView.prototype.isTerm = true;
    RulesTreeView.prototype.initialize = function() {
      _.bindAll(this, 'addOne', 'addAll', 'render');
      console.log(app.collections.rulesTree);
      app.collections.rulesTree.bind('add', this.addOne);
      app.collections.rulesTree.bind('refresh', this.addAll);
      app.collections.rulesTree.bind('all', this.renderList);
      return app.collections.rulesTree.fetch();
    };
    RulesTreeView.prototype.addOne = function(rule) {
      var view;
      view = new RulesTreeItemView({
        model: rule,
        isTerm: this.isTerm
      });
      this.isTerm = !this.isTerm;
      return $(this.el).append(view.render().el);
    };
    RulesTreeView.prototype.addAll = function() {
      return app.collections.rulesTree.each(this.addOne);
    };
    RulesTreeView.prototype.renderList = function() {
      return app.views.rulesTree.render();
    };
    return RulesTreeView;
  })();
}).call(this);
