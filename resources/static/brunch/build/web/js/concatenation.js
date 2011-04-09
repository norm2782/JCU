(function() {
  var HomeView, MainController, Rule, Rules, RulesListView, RulesTreeView;
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
      app.controllers.main = new MainController();
      app.views.home = new HomeView();
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
  Rules = (function() {
    function Rules() {
      Rules.__super__.constructor.apply(this, arguments);
    }
    __extends(Rules, Backbone.Collection);
    Rules.prototype.model = Rule;
    Rules.prototype.initialize = function() {
      return this.localStorage = new Store("rules");
    };
    Rules.prototype.done = function() {
      return this.filter(function(rule) {
        return rule.get('done');
      });
    };
    Rules.prototype.remaining = function() {
      return this.without.apply(this, this.done());
    };
    Rules.prototype.nextOrder = function() {
      if (!this.length) {
        return 1;
      }
      return this.last().get('order') + 1;
    };
    Rules.prototype.comparator = function(rule) {
      return rule.get('order');
    };
    Rules.prototype.clearCompleted = function() {
      return _.each(this.done(), function(rule) {
        return rule.clear();
      });
    };
    return Rules;
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
      app.collections.rules - list.bind('add', this.addOne);
      app.collections.rules - list.bind('refresh', this.addAll);
      return app.collections.rules - list.bind('all', this.renderStats);
    };
    RulesListView.prototype.render = function() {
      $(this.el).html(app.templates.rules - list());
      return this;
    };
    RulesListView.prototype.addOne = function(rule) {
      var view;
      view = new RuleListView({
        model: rule
      });
      return $(this.el).find("#rules-list").append(view.render().el);
    };
    RulesListView.prototype.addAll = function() {
      return app.collections.rules - list.each(this.addOne);
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
      app.collections.rules - tree.bind('add', this.addOne);
      app.collections.rules - tree.bind('refresh', this.addAll);
      return app.collections.rules - tree.bind('all', this.renderStats);
    };
    RulesTreeView.prototype.render = function() {
      $(this.el).html(app.templates.rules - tree());
      return this;
    };
    RulesTreeView.prototype.addOne = function(rule) {
      var view;
      view = new RuleTreeView({
        model: rule
      });
      return $(this.el).find("#rules-tree").append(view.render().el);
    };
    RulesTreeView.prototype.addAll = function() {
      return app.collections.rules - tree.each(this.addOne);
    };
    RulesTreeView.prototype.renderStats = function() {
      return app.views.stats.render();
    };
    return RulesTreeView;
  })();
}).call(this);
