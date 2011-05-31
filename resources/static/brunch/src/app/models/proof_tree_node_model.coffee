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

  setUnified: (tr) =>
    @setTerm(tr.term)
    childNo = tr.childTerms.length
    newChildren = new Array()
    if childNo > 0
      for i in [1..childNo]
        trLvl = @get('treeLvl').slice(0)
        trLvl.push(i)

        nd = new ProofTreeNode({ term: tr.childTerms[i-1].term
                               , treeLvl: trLvl
                               , treeLbl: @get('treeLbl') + "." + i
                               , validSyntax: true
                               , disabled: true })

        nd.setUnified tr.childTerms[i-1]
        newChildren.push(nd)
    @childTerms().refresh(newChildren)
