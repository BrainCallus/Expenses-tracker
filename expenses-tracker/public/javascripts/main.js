function showMessage(message) {
    $.jGrowl(message, {
        position: 'bottom-right',
        life: 15000,
        theme: "expenses_tracker"
    });
}

function processEnableErrorAutofocus() {
    let found = false;

    $(".enable-error-autofocus .field-error").each(function () {
        if (!found) {
            const val = $.trim($(this).text());
            if (val.length > 0) {
                let classes = this.className.split(" ");
                for (let i in classes) {
                    if (classes[i].match(/for__.*/)) {
                        const name = classes[i].substring("for__".length);
                        $(".enable-error-autofocus input[name=" + name + "]").focus();
                        $(".enable-error-autofocus select[name=" + name + "]").focus();
                        $(".enable-error-autofocus textarea[name=" + name + "]").focus();
                        found = true;
                    }
                }
            }
        }
    });

    return found;
}

function processEnableAutofocus(shouldBeEmpty) {
    let focusedField = null;

    $(".enable-autofocus input, .enable-autofocus select, .enable-autofocus textarea").each(function () {
        const elem = $(this);
        if (focusedField === null
            && elem.attr("type") !== "hidden"
            && elem.attr("type") !== "checkbox"
            && !elem.hasClass("skip-autofocus")
            && $(elem).is(':visible')) {
            if (shouldBeEmpty && (elem.val() === null || elem.val() === "")) {
                focusedField = elem;
            } else if (!shouldBeEmpty) {
                focusedField = elem;
            }
        }
    });

    if (focusedField !== null) {
        focusedField.focus();
    }
}

function runAutofocus() {
    const found = processEnableErrorAutofocus();
    if (!found) {
        processEnableAutofocus(false);
    }
}

$(document).ready(function () {
    runAutofocus();
});