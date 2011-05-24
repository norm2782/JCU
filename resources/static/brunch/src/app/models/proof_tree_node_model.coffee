class exports.ProofTreeNode extends Backbone.Model
  # Available attributes:
  # term :: String
  # mcid :: String
  # childTerms :: BackBone.Collection
  # proofResult :: String, can be either Correct, Incomplete or Invalid

  initialize: =>
    @set { childTerms: new Backbone.Collection()
         , mcid: @cid }

  term: =>
    @get('term')

  proofResult: =>
    @get('proofResult')

  isProved: =>
    f = (acc, nd) -> nd.isProved() && acc
    (@proofResult() == "Correct") && @childTerms().reduce f, true

  setTerm: (tm) =>
    @set({term: tm})

  childTerms: =>
    @get('childTerms')

  setChildren: (data) =>
    childNo = data.children
    # TODO: grab either rhss or urhss from data, depending on some global
    # setting, and insert it as term in the new nodes
    if childNo > 0
      newChildren = new Array()
      for i in [1..childNo]
        newChildren.push(new ProofTreeNode({ term: data.urhss[i - 1]
                                           , treeLvl: @get('treeLvl') + 1
                                           , treeLbl: @get('treeLbl') + "." + i
                                           , validSyntax: true }))
      @childTerms().refresh(newChildren)

  setValidSyntax: (flag) =>
    @set({validSyntax: flag})

  hasValidSyntax: =>
    @get('validSyntax')

  isValid: =>
    f = (acc, nd) -> nd.isValid() && acc
    @hasValidSyntax() && @childTerms().reduce f, true

  setProofResult: (data) =>
    @set({proofResult: data.proofCheckResult})
    @trigger('proof')
    i = 0
    f = (x) ->
      x.setProofResult data.proofCheckChildren[i]
      i++
    @childTerms().each f

  reset: =>
     @childTerms().refresh new Array()
