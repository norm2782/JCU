(function() {
  var HomeView, MainController, Rule, RuleListItemView, RuleTreeItemView, RulesList, RulesListView, RulesTree, RulesTreeView;
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
      content: 'empty rule...'
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
      $(this.el).find('#rules-tree-div').append(app.views.ruleTree.render().el);
      $(this.el).find('#rules-list-div').append(app.views.ruleList.render().el);
      return this;
    };
    return HomeView;
  })();
  RuleListItemView = (function() {
    function RuleListItemView() {
      this.update = __bind(this.update, this);;
      this.render = __bind(this.render, this);;      RuleListItemView.__super__.constructor.apply(this, arguments);
    }
    __extends(RuleListItemView, Backbone.View);
    RuleListItemView.prototype.tagName = "li";
    RuleListItemView.prototype.events = {
      'click .check': 'toggleDone',
      'dblclick .todo-content': 'edit',
      'click .todo-destroy': 'clear',
      'keypress .todo-input': 'updateOnEnter'
    };
    RuleListItemView.prototype.initialize = function() {
      this.model.bind('change', this.render);
      return this.model.view = this;
    };
    RuleListItemView.prototype.render = function() {
      this.$(this.el).html(app.templates.todo({
        todo: this.model.toJSON()
      }));
      this.$('.todo-input').bind('blur', this.update);
      return this;
    };
    RuleListItemView.prototype.toggleDone = function() {
      return this.model.toggle();
    };
    RuleListItemView.prototype.edit = function() {
      this.$(this.el).addClass("editing");
      return $('.todo-input').focus();
    };
    RuleListItemView.prototype.update = function() {
      this.model.save({
        content: this.$('.todo-input').val()
      });
      return this.$(this.el).removeClass("editing");
    };
    RuleListItemView.prototype.updateOnEnter = function(e) {
      if (e.keyCode === $.ui.keyCode.ENTER) {
        return this.update();
      }
    };
    RuleListItemView.prototype.remove = function() {
      return $(this.el).remove();
    };
    RuleListItemView.prototype.clear = function() {
      return this.model.clear();
    };
    return RuleListItemView;
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
  RuleTreeItemView = (function() {
    function RuleTreeItemView() {
      this.update = __bind(this.update, this);;
      this.render = __bind(this.render, this);;      RuleTreeItemView.__super__.constructor.apply(this, arguments);
    }
    __extends(RuleTreeItemView, Backbone.View);
    RuleTreeItemView.prototype.tagName = "li";
    RuleTreeItemView.prototype.events = {
      'click .check': 'toggleDone',
      'dblclick .todo-content': 'edit',
      'click .todo-destroy': 'clear',
      'keypress .todo-input': 'updateOnEnter'
    };
    RuleTreeItemView.prototype.initialize = function() {
      this.model.bind('change', this.render);
      return this.model.view = this;
    };
    RuleTreeItemView.prototype.render = function() {
      this.$(this.el).html(app.templates.todo({
        todo: this.model.toJSON()
      }));
      this.$('.todo-input').bind('blur', this.update);
      return this;
    };
    RuleTreeItemView.prototype.toggleDone = function() {
      return this.model.toggle();
    };
    RuleTreeItemView.prototype.edit = function() {
      this.$(this.el).addClass("editing");
      return $('.todo-input').focus();
    };
    RuleTreeItemView.prototype.update = function() {
      this.model.save({
        content: this.$('.todo-input').val()
      });
      return this.$(this.el).removeClass("editing");
    };
    RuleTreeItemView.prototype.updateOnEnter = function(e) {
      if (e.keyCode === $.ui.keyCode.ENTER) {
        return this.update();
      }
    };
    RuleTreeItemView.prototype.remove = function() {
      return $(this.el).remove();
    };
    RuleTreeItemView.prototype.clear = function() {
      return this.model.clear();
    };
    return RuleTreeItemView;
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
