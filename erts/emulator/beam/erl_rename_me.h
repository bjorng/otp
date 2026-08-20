typedef struct {
    Eterm module;               /* Key */
    Eterm name;                 /* Key */

    ErtsExtModRec states[ERTS_ADDRESSV_SIZE];
} ErtsExtRecord;

typedef ErtsExtModRec {
    /* Literal-tagged CONS pointer if the module is loaded. The head
     * of the CONS holds to pointer to the canonical
     * ErtsRecordDefinition for each code generation, while the tail
     * holds the default values.
     *
     * THE_NON_VALUE if the module is not loaded.
     */
    Eterm cons;

    Hash rec_ops;               /* Hash table of ErtExtRecOp */
};

typedef struct {
    Eterm code;

    enum {
        create,                 /* Uses cons for creation. */
        update,                 /* Only compares cons. */
        get_elements            /* Only compares cons. */
    } code_type;                /* Key */
    
    /* FieldName0 Dst0 FieldName1 Dst1 ... FieldNameN DstN;
     * sorted in atom order of field names. */
    Eterm names_and_dest[];     /* Key */
} ErtsExtRecOp;
