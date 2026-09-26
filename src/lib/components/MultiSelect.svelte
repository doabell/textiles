<script lang="ts">
    import { Check, ChevronDown, Search, X } from "@lucide/svelte";
    import { tick } from "svelte";
    let {
        label,
        options,
        value = $bindable<string[]>([]),
        placeholder = "All",
        disabled = false,
    }: {
        label: string;
        options: string[];
        value?: string[];
        placeholder?: string;
        disabled?: boolean;
    } = $props();
    const id = $props.id();
    let open = $state(false);
    let query = $state("");
    let root: HTMLDivElement;
    let searchInput = $state<HTMLInputElement>();
    let trigger: HTMLButtonElement;
    const matches = $derived(
        options.filter((option) =>
            option.toLocaleLowerCase().includes(query.trim().toLocaleLowerCase()),
        ),
    );
    function toggle(option: string) {
        value = value.includes(option)
            ? value.filter((item) => item !== option)
            : [...value, option];
    }
    async function removeSelection(option: string) {
        toggle(option);
        await tick();
        trigger?.focus();
    }
    async function toggleOpen() {
        open = !open;
        query = "";
        if (open) {
            await tick();
            searchInput?.focus({ preventScroll: true });
        }
    }
    function keyScope(node: HTMLElement) {
        node.addEventListener("keydown", keyboard);
        return {
            destroy() {
                node.removeEventListener("keydown", keyboard);
            },
        };
    }
    function keyboard(event: KeyboardEvent) {
        if (event.key === "Escape" && open) {
            event.preventDefault();
            event.stopPropagation();
            open = false;
            trigger?.focus();
        }
    }
</script>

<svelte:window
    onpointerdown={(event) => {
        if (open && root && !root.contains(event.target as Node)) open = false;
    }}
/>
<div
    class="multi-select"
    bind:this={root}
    use:keyScope
    role="group"
    aria-labelledby={id + "-label"}
>
    <span class="field-label" id={id + "-label"}>{label}</span>
    <button
        class="select-trigger"
        type="button"
        bind:this={trigger}
        {disabled}
        aria-labelledby={id + "-label " + id + "-value"}
        aria-expanded={open}
        aria-controls={id + "-options"}
        onclick={toggleOpen}
    >
        <span id={id + "-value"}
            >{value.length
                ? value.length === 1
                    ? value[0]
                    : `${value.length} selected`
                : placeholder}</span
        ><ChevronDown size={15} class={open ? "rotated" : ""} />
    </button>
    {#if open}
        <div class="options-panel" id={id + "-options"}>
            <div class="option-search">
                <Search size={15} /><input
                    bind:this={searchInput}
                    type="search"
                    bind:value={query}
                    placeholder="Search options"
                    aria-label="Search options"
                />
            </div>
            <div class="option-list">
                {#each matches as option (option)}
                    <label class="option" class:checked={value.includes(option)}>
                        <input
                            type="checkbox"
                            checked={value.includes(option)}
                            onchange={() => toggle(option)}
                        />
                        <span class="check-box" aria-hidden="true"
                            >{#if value.includes(option)}<Check size={13} />{/if}</span
                        >
                        <span>{option}</span>
                    </label>
                {:else}<p class="no-options">No matches</p>{/each}
            </div>
            <div class="option-actions">
                <button type="button" disabled={!value.length} onclick={() => (value = [])}
                    >Clear</button
                ><button
                    type="button"
                    onclick={() => {
                        open = false;
                        trigger?.focus();
                    }}>Done</button
                >
            </div>
        </div>
    {/if}
    {#if value.length}
        <ul class="selected-values" aria-label="Selected values">
            {#each value as option (option)}<li>
                    <span>{option}</span><button
                        type="button"
                        aria-label="Remove selection"
                        title={option}
                        onclick={() => removeSelection(option)}><X size={12} /></button
                    >
                </li>{/each}
        </ul>
    {/if}
</div>

<style>
    .multi-select {
        min-width: 0;
        font-family: var(--sans);
    }
    .field-label {
        display: block;
        margin-bottom: 0.5rem;
        color: var(--ink-soft);
        font: var(--type-label);
    }
    button {
        color: var(--ink);
    }
    .select-trigger {
        width: 100%;
        min-height: 2.8rem;
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 0.75rem;
        padding: 0.7rem 0.8rem;
        border: 1px solid var(--line-strong);
        background: transparent;
        font-size: 0.875rem;
        text-align: left;
    }
    .select-trigger > span {
        min-width: 0;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
    }
    .select-trigger :global(svg) {
        flex-shrink: 0;
    }
    .select-trigger :global(.rotated) {
        transform: rotate(180deg);
    }
    .select-trigger[aria-expanded="true"] {
        border-color: var(--ink);
    }
    .options-panel {
        border: 1px solid var(--ink);
        border-top: 0;
        background: var(--paper);
    }
    .option-search {
        display: flex;
        align-items: center;
        gap: 0.55rem;
        padding: 0.5rem 0.75rem;
        border-bottom: 1px solid var(--line);
    }
    .option-search input {
        width: 100%;
        min-width: 0;
        padding: 0.4rem 0;
        border: 0;
        background: transparent;
        font-size: 1rem;
    }
    .option-list {
        max-height: 14rem;
        overflow-y: auto;
        overscroll-behavior: contain;
        padding: 0.3rem;
    }
    .option {
        position: relative;
        display: flex;
        align-items: center;
        gap: 0.65rem;
        min-height: 2.65rem;
        padding: 0.45rem 0.4rem;
        font-size: 0.875rem;
        line-height: 1.35;
        cursor: pointer;
    }
    .option:hover,
    .option:focus-within {
        background: var(--paper-deep);
    }
    .option input {
        position: absolute;
        opacity: 0;
        width: 1px;
        height: 1px;
    }
    .check-box {
        display: grid;
        place-items: center;
        flex: 0 0 1rem;
        width: 1rem;
        height: 1rem;
        border: 1px solid var(--line-strong);
    }
    .checked .check-box {
        color: var(--paper);
        background: var(--ink);
        border-color: var(--ink);
    }
    .option:focus-within {
        outline: 2px solid var(--madder);
        outline-offset: -2px;
    }
    .option-actions {
        display: flex;
        justify-content: space-between;
        border-top: 1px solid var(--line);
    }
    .option-actions button {
        min-height: 2.75rem;
        padding: 0.5rem 0.8rem;
        border: 0;
        background: transparent;
        font-size: 0.8125rem;
    }
    .option-actions button:last-child {
        font-weight: 600;
    }
    .selected-values {
        display: flex;
        flex-wrap: wrap;
        gap: 0.35rem;
        margin: 0.55rem 0 0;
        padding: 0;
        list-style: none;
    }
    .selected-values li {
        display: flex;
        align-items: center;
        gap: 0.2rem;
        min-width: 0;
        max-width: 100%;
        padding-left: 0.45rem;
        background: var(--paper-deep);
        font-size: 0.75rem;
    }
    .selected-values li > span {
        overflow-wrap: anywhere;
    }
    .selected-values button {
        display: grid;
        place-items: center;
        flex-shrink: 0;
        width: 1.75rem;
        height: 1.75rem;
        border: 0;
        background: transparent;
    }
    .no-options {
        margin: 0.5rem;
        font-size: 0.875rem;
    }
</style>
