# Presentation Restructure Notes

Captured from voice feedback session - December 8, 2025

---

## Part 1: The Setup - Major Changes Needed

### NEW SLIDE: Why Claude Code

**Key points to cover:**
- We chose Claude Code specifically
- Things we DON'T want to do:
  - Don't want to use an IDE
  - Don't want to use external web-based tools
  - Want to stick with ONE coding agent
- Best all-around agent (Chris's opinion): Claude Code with Opus 4.5
- Challenge: Never coded in R before
- R is not a known specialty for AI coding agents
- Solution: Set up custom skills, agents, and a special planning command to get best results

### NEW SLIDE: What is a Skill? What are Subagents?

**Key points to cover:**
- Explain what a "skill" is in Claude Code context
- Explain what "subagents" are
- Briefly touch on plugins:
  - Why we didn't make a plugin for this
  - But acknowledge that we could have

### Slide: Building the R Data Science Skill

- Show the actual skill we created
- How it helps Claude understand R patterns

### Slide: Specialized Agents

- WHY we chose these specific agents
- What problems they solve

### Voice-to-Code Workflow

- Keep as is (working well)

---

## Part 2: Diabetes Dashboard Section - Reframing Needed

### First Slide After Part 2 Intro: "Why We Chose This"

**New narrative:**
- This was a QUICK DEMO to get our footing with R
- Goal: Data acquisition pipeline → ML model → results
- NOT trying to make it perfect
- Real question: Can we execute a WHOLE pipeline?
  - Data acquisition
  - Training process
  - Deduction
  - Analysis
- WITHOUT:
  - Going into an IDE
  - Using Jupyter notebooks
  - Opening up Posit/RStudio

### Dataset Choice Slide

- Brief chat with agent led to this dataset
- There are multiple diabetes datasets available
- We chose this one (explain why briefly)

### Model Performance Slide - REFRAME

**Current problem:** Focuses too much on the diabetes results

**New framing:**
- We weren't trying to get the best score possible
- We just wanted to:
  - Set it up
  - Get results
  - See if we could properly interpret
  - See if everything makes sense
- WITHOUT loading up interpretability frameworks or extra tools

**KEY INSIGHT (emphasize this):**
> "The key insight is NOT about the diabetes. The key insight is using AI to create the model. We were able to successfully create the model without having to use traditional resources."

- Back and forth talks with AI agent who:
  - Ran the model
  - Did the tweaking and tuning
- Important: Built without traditional methods and means

### Fairness Audit Slide - REFRAME

**New framing:**
- Question: Could we conduct a fairness audit with the AI agent?
- Answer: YES
- Here's the results
- (Keep it brief - the point is capability demonstration)

### NEW SLIDE: Transition to Dashboard

**Narrative:**
- "It didn't take us very long to do the model..."
- New question emerged: Can we do a visual communication piece?
- Could we go FURTHER with the agent and build a dashboard?

### Diabetes Dashboard Slide

**Needs:**
- Link to Hugging Face dashboard (user will provide)
- Screenshot of the dashboard (user will provide or we grab one)
- Key message: We were able to go from:
  - ML model → Dashboard → Visual data communication
- ALL done through voice communication with the agent
- No traditional toolsets required

---

## Part 3: RespiWatch Section - Refinements

### Slide 16: Section Header

- Keep as is

### Slide 17: "Then We Started Wiring In Data" - REFRAME

**Current title suggests:** Wiring in data

**New narrative:**
- "We started SEARCHING for data"
- Started prototyping the dashboard
- Would find data → but then find gaps
- Realized: Our formulas/models weren't strong because of:
  - Huge gaps in data
  - No data available for certain things

### The Revelation Slide - REFRAME

**Current:** "Wastewater Revelation"

**New framing:** "The Fallback Revelation"
- It wasn't JUST wastewater
- We realized there were OTHER sources beyond the "big 5":
  - WHO
  - CDC
  - ECDC
  - etc.
- Alternative sources discovered:
  - State and local sources
  - Wastewater surveillance
  - Other alternate sources
- New question: What if we could pull ALL of these together?
- Could we get the AI agent to aggregate them?

### Rt Estimation Slide

**Question to investigate:**
- Look at the code shown
- Is that ALL the code it takes in R to do Rt estimation?
- If yes, that's interesting and worth highlighting
- "Look how concise this is"

---

## Action Items

1. [ ] Read current index.qmd to understand exact slide structure
2. [ ] Create new slides for Part 1 (Why Claude Code, Skills/Subagents)
3. [ ] Reframe Part 2 diabetes slides with new narrative
4. [ ] Add transition slide before dashboard
5. [ ] Get Hugging Face dashboard link/screenshot from user
6. [ ] Reframe Part 3 revelation from "wastewater" to "fallback"
7. [ ] Check Rt estimation code - is it really that concise?
8. [ ] Brainstorm additional visual enhancements

---

## Questions for User

1. Can you provide the Hugging Face dashboard link?
2. Do you have a screenshot of the diabetes dashboard, or should I capture one?
3. For the "Why Claude Code" slide - any specific comparisons to other tools you want mentioned?
4. The Rt estimation code - should we verify it works/show output?
