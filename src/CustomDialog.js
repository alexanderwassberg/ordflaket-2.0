class CustomDialog extends HTMLElement {
    constructor() {
        super();

        this.attachShadow({ mode: 'open' });

        this.shadowRoot.innerHTML = `
            <dialog>
                <button id="closeButton">Close</button>
                <slot></slot>
            </dialog>
        `;

        this.dialog = this.shadowRoot.querySelector('dialog');
        this.closeButton = this.shadowRoot.querySelector('#closeButton');
    }

    connectedCallback() {
        if (this.closeButton) {
            this.closeButton.addEventListener('click', () => this.closeDialog());
        }
    }

    disconnectedCallback() {
        if (this.closeButton) {
            this.closeButton.removeEventListener('click', this.closeDialog);
        }
    }

    openDialog() {
        this.dialog.showModal();
    }

    closeDialog() {
        this.dialog.close();
        if (this.onClose) {
            this.onClose();
        }
    }
}

customElements.define('custom-dialog', CustomDialog);
