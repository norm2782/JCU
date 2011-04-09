(function() {
  var HomeView, MainController, Rule, RulesList, RulesListView, RulesTree, RulesTreeView;
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
  app.templates = {};
  $(document).ready(function() {
    app.initialize = function() {
      app.collections.rulesList = new RulesList();
      app.collections.rulesTree = new RulesTree();
      app.controllers.main = new MainController();
      app.views.home = new HomeView();
      app.views.ruleList = new RulesListView();
      app.views.ruleTree = new RulesTreeView();
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
    Rule.prototype.defaults = {
      content: 'empty rule...',
      isTerm: true
    };
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
    RulesList.prototype.done = function() {
      return this.filter(function(rule) {
        return rule.get('done');
      });
    };
    RulesList.prototype.remaining = function() {
      return this.without.apply(this, this.done());
    };
    RulesList.prototype.nextOrder = function() {
      if (!this.length) {
        return 1;
      }
      return this.last().get('order') + 1;
    };
    RulesList.prototype.comparator = function(rule) {
      return rule.get('order');
    };
    RulesList.prototype.clearCompleted = function() {
      return _.each(this.done(), function(rule) {
        return rule.clear();
      });
    };
    return RulesList;
  })();
  RulesTree = (function() {
    function RulesTree() {
      RulesTree.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesTree, Backbone.Collection);
    RulesTree.prototype.model = Rule;
    RulesTree.prototype.done = function() {
      return this.filter(function(rule) {
        return rule.get('done');
      });
    };
    RulesTree.prototype.remaining = function() {
      return this.without.apply(this, this.done());
    };
    RulesTree.prototype.nextOrder = function() {
      if (!this.length) {
        return 1;
      }
      return this.last().get('order') + 1;
    };
    RulesTree.prototype.comparator = function(rule) {
      return rule.get('order');
    };
    RulesTree.prototype.clearCompleted = function() {
      return _.each(this.done(), function(rule) {
        return rule.clear();
      });
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
      $(this.el).find('home-view').append(app.views.ruleTree.render().el);
      $(this.el).find('home-view').append(app.views.ruleList.render().el);
      return this;
    };
    return HomeView;
  })();
  RulesListView = (function() {
    function RulesListView() {
      this.renderStats = __bind(this.renderStats, this);;
      this.addAll = __bind(this.addAll, this);;
      this.addOne = __bind(this.addOne, this);;      RulesListView.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesListView, Backbone.View);
    RulesListView.prototype.id = 'rules-list-view';
    RulesListView.prototype.initialize = function() {
      app.collections.rulesList.bind('add', this.addOne);
      app.collections.rulesList.bind('refresh', this.addAll);
      return app.collections.rulesList.bind('all', this.renderStats);
    };
    RulesListView.prototype.render = function() {
      $(this.el).html(app.templates.rulesList());
      return this;
    };
    RulesListView.prototype.addOne = function(rule) {
      var view;
      view = new RulesListView({
        model: rule
      });
      return $(this.el).find("#rules-list").append(view.render().el);
    };
    RulesListView.prototype.addAll = function() {
      return app.collections.rulesList.each(this.addOne);
    };
    RulesListView.prototype.renderStats = function() {
      return app.views.stats.render();
    };
    return RulesListView;
  })();
  RulesTreeView = (function() {
    function RulesTreeView() {
      this.renderStats = __bind(this.renderStats, this);;
      this.addAll = __bind(this.addAll, this);;
      this.addOne = __bind(this.addOne, this);;      RulesTreeView.__super__.constructor.apply(this, arguments);
    }
    __extends(RulesTreeView, Backbone.View);
    RulesTreeView.prototype.id = 'rules-tree-view';
    RulesTreeView.prototype.initialize = function() {
      app.collections.rulesTree.bind('add', this.addOne);
      app.collections.rulesTree.bind('refresh', this.addAll);
      return app.collections.rulesTree.bind('all', this.renderStats);
    };
    RulesTreeView.prototype.render = function() {
      $(this.el).html(app.templates.rulesTree());
      return this;
    };
    RulesTreeView.prototype.addOne = function(rule) {
      var view;
      view = new RulesTreeView({
        model: rule
      });
      return $(this.el).find("#rules-tree").append(view.render().el);
    };
    RulesTreeView.prototype.addAll = function() {
      return app.collections.rulesTree.each(this.addOne);
    };
    RulesTreeView.prototype.renderStats = function() {
      return app.views.stats.render();
    };
    return RulesTreeView;
  })();
}).call(this);
