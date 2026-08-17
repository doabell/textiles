<script lang="ts">
    import { projectTools } from "$lib/data/projects";

    let {
        active,
    }: { active: "textile-geographies" | "textiles-modifiers-and-values" | "swatch-search" } =
        $props();

    const tools = [
        projectTools.find((tool) => tool.slug === "textile-geographies"),
        projectTools.find((tool) => tool.slug === "textiles-modifiers-and-values"),
        projectTools.find((tool) => tool.slug === "swatch-search"),
    ].filter((tool): tool is NonNullable<typeof tool> => Boolean(tool));

    const current = $derived(tools.find((tool) => tool.slug === active) ?? tools[0]!);
</script>

<header class="research-header page-shell">
    <div class="research-title">
        <p class="eyebrow">Data Visualization</p>
        <h1>{current.eyebrow}</h1>
    </div>
    <p class="research-description">{current.description}</p>
    <nav class="research-tabs" aria-label="Data visualization applications">
        {#each tools as tool}
            <a
                class:active={tool.slug === active}
                aria-current={tool.slug === active ? "page" : undefined}
                href={tool.href}
            >
                {tool.eyebrow}
            </a>
        {/each}
    </nav>
</header>

<style>
    .research-header {
        display: grid;
        grid-template-columns: minmax(0, 1.25fr) minmax(20rem, 0.75fr);
        gap: clamp(2.5rem, 7vw, 8rem);
        align-items: end;
        padding-top: clamp(2.8rem, 6vw, 5rem);
        padding-bottom: 0;
    }

    .research-title h1 {
        max-width: 12ch;
        margin: 0;
        font-family: var(--serif);
        font-size: clamp(3rem, 5.7vw, 5.9rem);
        font-weight: 400;
        letter-spacing: -0.057em;
        line-height: 0.9;
    }

    .research-description {
        max-width: 36rem;
        margin: 0 0 0.25rem;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: clamp(0.96rem, 1.15vw, 1.08rem);
        line-height: 1.62;
    }

    .research-tabs {
        grid-column: 1 / -1;
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        margin-top: clamp(1.8rem, 3.5vw, 3rem);
        border: 1px solid var(--line-strong);
        border-bottom: 0;
    }

    .research-tabs a {
        display: flex;
        align-items: center;
        min-height: 3.7rem;
        padding: 0.85rem 1.15rem;
        color: var(--ink-soft);
        border-left: 1px solid var(--line-strong);
        font-size: 0.84rem;
        font-weight: 650;
        line-height: 1.25;
        text-decoration: none;
    }

    .research-tabs a:first-child {
        border-left: 0;
    }

    .research-tabs a:hover {
        color: var(--ink);
        background: var(--paper-deep);
    }

    .research-tabs a.active {
        color: var(--paper);
        background: var(--ink);
    }

    @media (max-width: 760px) {
        .research-header {
            grid-template-columns: 1fr;
            align-items: start;
        }

        .research-description {
            max-width: 42rem;
        }

        .research-tabs {
            grid-column: 1;
            grid-template-columns: 1fr;
        }

        .research-tabs a {
            min-height: 3.7rem;
            border-top: 1px solid var(--line-strong);
            border-left: 0;
        }

        .research-tabs a:first-child {
            border-top: 0;
        }
    }
</style>
