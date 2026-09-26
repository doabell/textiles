/** Natural-height masonry with source-order keyboard navigation. */
export function waterfall(node: HTMLElement, enabled = true) {
    let frame = 0;
    const reset = () => {
        node.style.gridAutoRows = "";
        for (const item of node.children)
            if (item instanceof HTMLElement) {
                item.style.gridRowEnd = "";
                item.style.alignSelf = "";
            }
    };
    const layout = () => {
        cancelAnimationFrame(frame);
        frame = requestAnimationFrame(() => {
            if (!enabled) {
                reset();
                return;
            }
            node.style.gridAutoRows = "4px";
            const gap = parseFloat(getComputedStyle(node).rowGap) || 0;
            for (const item of node.children) {
                if (!(item instanceof HTMLElement)) continue;
                item.style.alignSelf = "start";
                item.style.gridRowEnd = "";
                item.style.gridRowEnd =
                    "span " +
                    Math.max(1, Math.ceil((item.getBoundingClientRect().height + gap) / (4 + gap)));
            }
        });
    };
    const observer = new ResizeObserver(layout);
    const watch = () => {
        observer.disconnect();
        observer.observe(node);
        for (const item of node.children) observer.observe(item);
        layout();
    };
    const mutation = new MutationObserver(watch);
    mutation.observe(node, { childList: true });
    node.addEventListener("load", layout, true);
    document.fonts.ready.then(layout);
    watch();
    return {
        update(next: boolean) {
            enabled = next;
            layout();
        },
        destroy() {
            cancelAnimationFrame(frame);
            observer.disconnect();
            mutation.disconnect();
            node.removeEventListener("load", layout, true);
        },
    };
}
