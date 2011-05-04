window.app = {}
app.controllers = {}
app.models = {}
app.collections = {}
app.views = {}
app.styles = {}
app.templates = {}

RulesList = require('collections/rules_list_collection').RulesList
RuleTree = require('models/rule_tree').RuleTree
MainController = require('controllers/main_controller').MainController
HomeView = require('views/home_view').HomeView
RulesListView = require('views/rules_list_view').RulesListView
RulesTreeView = require('views/rules_tree_view').RulesTreeView

# app bootstrapping on document ready
$(document).ready ->
  app.initialize = ->
    app.collections.rulesList = new RulesList()

    # _.and :: [Bool] -> Bool
    _.mixin {and: (xs) -> _.all xs, _.identity}

    # _.or  :: [Bool] -> Bool
    _.mixin {or:  (xs) -> _.any xs, _.identity}

    app.controllers.main = new MainController()

    app.models.tree = new RuleTree()

    app.views.home = new HomeView()
    app.views.rulesList = new RulesListView()
    app.views.rulesTree = new RulesTreeView({model: app.models.tree})

    Backbone.history.saveLocation("home") if Backbone.history.getFragment() is ''
  app.initialize()
  Backbone.history.start()
