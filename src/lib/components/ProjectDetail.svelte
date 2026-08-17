<script lang="ts">
    import { ArrowLeft, ArrowRight, CheckCircle2, Info } from "@lucide/svelte";
    import type { ProjectTool } from "$lib/data/projects";

    let { tool }: { tool: ProjectTool } = $props();
</script>

<svelte:head>
    <title>{tool.title} — Dutch Textile Trade</title>
    <meta name="description" content={tool.description} />
</svelte:head>

<div class={`project-hero ${tool.accent}`}>
    <div class="page-shell">
        <a href="/data-visualization/"><ArrowLeft size={14} /> All research tools</a>
        <p class="eyebrow">{tool.eyebrow}</p>
        <h1>{tool.title}</h1>
        <p>{tool.description}</p>
        <a class="button" href={tool.href}>{tool.action} <ArrowRight size={16} /></a>
    </div>
</div>

<section class="instructions page-shell">
    <div>
        <p class="eyebrow">Application instructions</p>
        <h2>{tool.title}</h2>
        <p class="creators">{tool.creators}</p>
    </div>
    <ol>
        {#each tool.instructions as instruction}
            <li>
                <CheckCircle2 size={19} strokeWidth={1.4} />
                <p>{instruction}</p>
            </li>
        {/each}
    </ol>
</section>

<aside class="modifier-note">
    <div class="page-shell">
        <Info size={23} strokeWidth={1.35} />
        <div class="note-list">
            {#each tool.notes as note}
                <article>
                    <p class="eyebrow">{note.title}</p>
                    <p>{note.body}</p>
                </article>
            {/each}
        </div>
    </div>
</aside>

<style>
    .project-hero {
        padding: 1.2rem 0 clamp(5rem, 10vw, 10rem);
        color: var(--paper);
        background: var(--indigo-deep);
    }

    .project-hero.madder {
        background: var(--madder-dark);
    }

    .project-hero.saffron {
        color: var(--ink);
        background: var(--saffron);
    }

    .project-hero > div > a:first-child {
        display: inline-flex;
        align-items: center;
        gap: 0.4rem;
        margin-bottom: clamp(4rem, 9vw, 9rem);
        color: inherit;
        font-family: var(--sans);
        font-size: 0.56rem;
        letter-spacing: 0.07em;
        text-decoration: none;
        text-transform: uppercase;
    }

    .project-hero .eyebrow {
        color: var(--saffron);
    }

    .project-hero.saffron .eyebrow {
        color: var(--madder-dark);
    }

    h1 {
        max-width: 10ch;
        margin-bottom: 1.5rem;
        font-family: var(--serif);
        font-size: clamp(4rem, 9vw, 9rem);
        font-weight: 400;
        letter-spacing: -0.065em;
        line-height: 0.85;
    }

    .project-hero > div > p:not(.eyebrow) {
        max-width: 42rem;
        color: rgba(244, 239, 229, 0.72);
        font-family: var(--reading);
        font-size: clamp(1rem, 1.35vw, 1.14rem);
        line-height: 1.62;
    }

    .project-hero.saffron > div > p:not(.eyebrow) {
        color: rgba(23, 23, 17, 0.68);
    }

    .project-hero .button {
        margin-top: 1.2rem;
        color: var(--ink);
        border-color: var(--paper);
        background: var(--paper);
    }

    .project-hero.saffron .button {
        color: var(--paper);
        border-color: var(--ink);
        background: var(--ink);
    }

    .instructions {
        display: grid;
        grid-template-columns: minmax(20rem, 0.75fr) 1fr;
        gap: clamp(3rem, 9vw, 10rem);
        padding-top: clamp(4rem, 7vw, 7rem);
        padding-bottom: clamp(4rem, 7vw, 7rem);
    }

    .instructions h2 {
        margin-bottom: 1.5rem;
        font-family: var(--serif);
        font-size: clamp(3rem, 5vw, 5.4rem);
        font-weight: 400;
        letter-spacing: -0.055em;
        line-height: 0.95;
    }

    .creators {
        max-width: 33rem;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 0.85rem;
    }

    ol {
        margin: 0;
        padding: 0;
        border-top: 1px solid var(--line-strong);
        list-style: none;
    }

    li {
        display: grid;
        grid-template-columns: auto 1fr;
        gap: 1rem;
        padding: 1.6rem 0;
        border-bottom: 1px solid var(--line);
    }

    li :global(svg) {
        color: var(--madder);
    }

    li p {
        max-width: 65ch;
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 1rem;
        line-height: 1.64;
    }

    .modifier-note {
        padding: 4rem 0;
        color: var(--paper);
        background: var(--indigo-deep);
    }

    .modifier-note > div {
        display: grid;
        grid-template-columns: auto 1fr;
        gap: 1.5rem;
        align-items: start;
    }

    .modifier-note .eyebrow {
        color: var(--saffron);
    }

    .note-list {
        display: grid;
        gap: 2rem;
    }

    .note-list article + article {
        padding-top: 2rem;
        border-top: 1px solid rgba(244, 239, 229, 0.2);
    }

    .note-list article > p:last-child {
        max-width: 65ch;
        margin: 0;
        color: rgba(244, 239, 229, 0.7);
        font-family: var(--reading);
        font-size: 1rem;
        line-height: 1.65;
    }

    @media (max-width: 800px) {
        .instructions {
            grid-template-columns: 1fr;
        }
    }
</style>
