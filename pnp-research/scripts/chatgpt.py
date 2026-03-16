#!/usr/bin/env python3
"""
ChatGPT API caller for P ≠ NP research program.
Called by Claude Code to get brainstorming, planning, and formalization from GPT-4o.

Usage:
    python scripts/chatgpt.py --mode brainstorm --context "problem description"
    python scripts/chatgpt.py --mode plan --context "$(cat state/current_problem.md)"
    python scripts/chatgpt.py --mode formalize --context "rough idea"
    python scripts/chatgpt.py --mode review --context "$(cat state/program_state.md)"
    python scripts/chatgpt.py --mode custom --context "any prompt"

Requires: OPENAI_API_KEY environment variable
Install: pip install openai
"""

import argparse
import os
import sys
import json
from datetime import datetime
from pathlib import Path

import urllib.request
import ssl

# --- System prompts per mode ---

SYSTEM_PROMPTS = {
    "brainstorm": """You are a creative mathematical research collaborator working on a P ≠ NP proof program based on sheaf-theoretic non-locality and hardness magnification.

Your role is BRAINSTORMING. Be ambitious, speculative, and divergent. Generate 3-5 genuinely distinct angles of attack on the problem presented. For each angle:
- State the core idea in 2-3 sentences
- Identify what existing results it builds on
- Flag the main obstacle or risk
- Suggest a concrete first step to test feasibility

Do NOT self-censor for difficulty. The verification partner (Claude) will check correctness later. Your job is to generate promising directions, including high-risk/high-reward ones.

Do NOT be generic. Every suggestion must engage with the specific mathematical structures in the problem (presheaves, STRUCT-MATCH, Nullstellensatz degree, compatibility CSPs, etc.). If you don't know enough about a specific structure, say so rather than being vague.""",

    "plan": """You are a strategic research planner for a P ≠ NP proof program. You excel at organizing complex multi-path research into actionable roadmaps.

Your role is PLANNING. Given the current research state, produce:
1. A prioritized list of next steps (ranked by expected impact × feasibility)
2. A phased timeline with explicit decision gates
3. Cross-path synergies — how progress on one path informs others
4. Resource allocation — what deserves focused effort vs. background exploration
5. Explicit fallback plans — what to do if the primary approach fails

Be ambitious in scope but honest about difficulty. Use concrete milestones, not vague "explore this direction" language. Every milestone should have a success criterion and a timeline estimate.

Frame the plan as a research portfolio with diversified risk. Identify which paths hedge each other.""",

    "formalize": """You are a mathematical formalizer working on complexity theory, sheaf cohomology, and circuit lower bounds.

Your role is FORMALIZATION. Take the rough idea presented and produce:
1. Precise mathematical definitions
2. Formal conjecture statements with quantifiers
3. A proof sketch or proof strategy
4. Identification of which steps are standard vs. novel
5. Explicit dependencies — what must be true for this to work

Be aggressive — write the full formalization even if some steps are unproved. Mark unproved steps clearly with [UNVERIFIED] tags. The verification partner will check everything; your job is to get the complete structure on paper.

Use standard complexity theory notation. Define any non-standard terms.""",

    "review": """You are a strategic reviewer for a P ≠ NP research program. You assess the current state and recommend course corrections.

Your role is STRATEGIC REVIEW. Given the current program state, provide:
1. An honest assessment of each active path's viability (with probability estimates if possible)
2. What has changed since the last review and what it implies
3. Whether priorities should shift
4. What the program is doing right and what it's missing
5. Concrete recommendations for the next 2-4 weeks

Be constructive but unflinching. If a path is dying, say so. If the program is missing an obvious angle, point it out. Think about how this would look to a complexity theory referee.""",

    "custom": """You are a mathematical research collaborator working on a P ≠ NP proof program based on sheaf-theoretic non-locality and hardness magnification. Respond to the request below with your best mathematical and strategic thinking. Be specific, ambitious, and engage deeply with the technical content."""
}

RESEARCH_CONTEXT_HEADER = """
## Background Context for P ≠ NP Research Program

**Core approach:** Sheaf-theoretic framework encoding circuit compatibility as a presheaf on the sub-cube site. The obstruction detection function OD(T) detects when local circuits exist but cannot be globally merged. Via hardness magnification (Oliveira-Santhanam), proving OD ∉ SIZE[N^{1+ε}] implies P ≠ NP.

**Key structures:** MCSP presheaf F^{cir}_{T,s}, STRUCT-MATCH compatibility predicate (DAG isomorphism after hardwiring), compatibility CSP Γ(T,d,s), structural entropy ratio σ∞(d), cocycle variety V_T.

**Proved:** No circuit polymorphism (Result A), compatible families for parity (Result B), Ω(N/log N) non-locality (Result C), sub-cube query Ω(N/poly(n)), SAT_R ∉ AC⁰ (large k), non-relativization, non-algebrization.

**Refuted:** Sub-cube lifting (counterexample: MAJ∘PARITY), Fourier for STRUCT-MATCH, parity anchor density, direct misalignment→hardness, stiffness as intrinsic.

**Active paths:** A (Atserias-Müller, LOW priority, 5 obstacles), B (structural anatomy paper, MEDIUM, ready), C (proof complexity / NS degree, HIGH, needs Macaulay2).

**Core bottleneck:** No known bridge from structural/query-theoretic hardness to circuit lower bounds for OD. The magnification gap.
"""


def call_chatgpt(mode: str, context: str, model: str = "gpt-4o", temperature: float = None) -> str:
    """Call ChatGPT API via urllib and return the response."""

    api_key = os.environ.get("OPENAI_API_KEY")
    if not api_key:
        return "ERROR: OPENAI_API_KEY environment variable not set."

    # Set temperature based on mode
    if temperature is None:
        temperature = {
            "brainstorm": 0.8,
            "plan": 0.4,
            "formalize": 0.3,
            "review": 0.5,
            "custom": 0.6
        }.get(mode, 0.6)

    system_prompt = SYSTEM_PROMPTS.get(mode, SYSTEM_PROMPTS["custom"])

    # Build the user message with research context
    user_message = f"{RESEARCH_CONTEXT_HEADER}\n\n---\n\n## Current Request\n\n{context}"

    payload = json.dumps({
        "model": model,
        "temperature": temperature,
        "max_tokens": 4096,
        "messages": [
            {"role": "system", "content": system_prompt},
            {"role": "user", "content": user_message}
        ]
    }).encode("utf-8")

    req = urllib.request.Request(
        "https://api.openai.com/v1/chat/completions",
        data=payload,
        headers={
            "Content-Type": "application/json",
            "Authorization": f"Bearer {api_key}",
        },
        method="POST",
    )

    try:
        ctx = ssl.create_default_context()
        with urllib.request.urlopen(req, context=ctx, timeout=120) as resp:
            data = json.loads(resp.read().decode("utf-8"))

        content = data["choices"][0]["message"]["content"]

        # Build metadata header
        usage = data.get("usage", {})
        tokens_in = usage.get("prompt_tokens", 0)
        tokens_out = usage.get("completion_tokens", 0)
        metadata = f"""---
mode: {mode}
model: {model}
temperature: {temperature}
timestamp: {datetime.now().isoformat()}
tokens_in: {tokens_in}
tokens_out: {tokens_out}
cost_estimate: ${(tokens_in * 2.5 + tokens_out * 10) / 1_000_000:.4f}
---

"""
        return metadata + content

    except urllib.error.HTTPError as e:
        body = e.read().decode("utf-8", errors="replace")
        return f"ERROR calling ChatGPT API: HTTP {e.code}\n{body}"
    except Exception as e:
        return f"ERROR calling ChatGPT API: {e}\n\nCheck that OPENAI_API_KEY is set correctly."


def main():
    parser = argparse.ArgumentParser(description="Call ChatGPT for P ≠ NP research")
    parser.add_argument("--mode", required=True, 
                        choices=["brainstorm", "plan", "formalize", "review", "custom"],
                        help="Type of request")
    parser.add_argument("--context", required=True,
                        help="The problem/context to send (string or use $(cat file))")
    parser.add_argument("--model", default="gpt-4o",
                        help="OpenAI model to use (default: gpt-4o)")
    parser.add_argument("--temperature", type=float, default=None,
                        help="Override temperature")
    parser.add_argument("--output", default="state/chatgpt_response.md",
                        help="Output file path")
    
    args = parser.parse_args()
    
    # Ensure output directory exists
    Path(args.output).parent.mkdir(parents=True, exist_ok=True)
    
    # Check API key
    if not os.environ.get("OPENAI_API_KEY"):
        print("ERROR: OPENAI_API_KEY environment variable not set.")
        print("Set it with: export OPENAI_API_KEY='your-key-here'")
        sys.exit(1)
    
    print(f"Calling ChatGPT ({args.model}) in {args.mode} mode...")
    
    response = call_chatgpt(
        mode=args.mode,
        context=args.context,
        model=args.model,
        temperature=args.temperature
    )
    
    # Write response
    with open(args.output, "w", encoding="utf-8") as f:
        f.write(response)
    
    print(f"Response written to {args.output}")
    print(f"Length: {len(response)} chars")


if __name__ == "__main__":
    main()
