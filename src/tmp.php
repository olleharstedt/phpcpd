<?php

class CloneUtils
{
    /**
     * @param Clone[]
     */
    private static function allFingerprintsEqual(array $clones): bool
    {
        /** @var string */
        $commonFingerprint = $clones[0]->getFingerprint();
        foreach ($clones as $clone) {
            if ($commonFingerprint !== $clone->getFingerprint()) {
                return false;
            }
        }
        return true;
    }

    /**
     * @param Clone[]
     */
    public static function createFingerprint(array $clones): string
    {
        assert(!empty($clones), "Cannot create fingerprint for empty collection of clones");

        if (self::allFingerprintsEqual($clones)) {
            return $clones[0]->getFingerprint();
        }

        // as long as the fingerprints of the individual clones do not change,
        // we need to assure that the clone class fingerprint does not change
        // either. in order to compute a stable fingerprint for a clone class,
        // we thus need to establish a stable order in which clone fingerprints
        // are added to the clone class fingerprint. since we want to be
        // independent of clone filenames or clone positions in files, we
        // compute this order directly on the fingerprints.
        $fingerprints = [];
        foreach ($clones as $clone) {
            $fingerprints[] = $clone->getFingerprint();
        }

        return md5(implode(' ', $fingerprints));
    }
}

abstract class KeyValueStoreBase
{
	/**
	 * Integer that identifies this value store. The scope of the id depends on
	 * the id provider used.
     * @var int
	 */
	private $id;

    /** Map that stores key-value pairs
     * @var array<string, object> */
	private $values = [];

    /** Map that stores transient flags
     * @var array<string, bool> */
	private $transientFlags = [];

	/** Construct store with set id */
	protected function __construct(int $id) {
		$this->id = $id;
	}

	/** Return the id of the store */
    public function getId(): int
    {
		return $this->id;
	}

	/** Stores a value in the {@link CloneClass} by using a keyword. */
    public function setValue(string $key, object $value): void
    {
		$this->values[key] = $value;
	}

	/** Gets a value */
    public function getValue(string $key): ?object
    {
		if ($this->values === null) {
			return null;
		}
		return $this->values[$key];
	}

	/** Checks whether a value is stored under this key */
    public function containsValue(string $key): bool
    {
		if ($this->values === null) {
			return false;
		}
		return array_key_exists($key, $this->values);
	}

}

class Clone extends KeyValueStoreBase {

	/** {@link CloneClass} this clone belongs to */
	private CloneClass cloneClass;

	/** The location of the clone. */
	private final TextRegionLocation location;

	/** Fingerprint of clone */
	private final String fingerprint;

	/** Position of the first unit of the clone in its element */
	private final int startUnitIndexInElement;

	/** Length of the clone in units */
	private final int lengthInUnits;

	/**
	 * The gaps in the clone stored as region of raw offsets (absolute in
	 * element).
	 */
	private List<Region> gaps;

	/** Delta size in units */
	protected int deltaInUnits;

	/** Creates a clone with a delta in units of 0 */
	public Clone(long id, CloneClass cloneClass, TextRegionLocation location,
			int startUnitIndexInElement, int lengthInUnits, String fingerprint) {
		this(id, cloneClass, location, startUnitIndexInElement, lengthInUnits,
				fingerprint, 0);
	}

	/**
	 * Constructor
	 * 
	 * @param cloneClass
	 *            this may be null to explicitly create a clone instance without
	 *            clone class.
	 */
	public Clone(long id, CloneClass cloneClass, TextRegionLocation location,
			int startUnitIndexInElement, int lengthInUnits, String fingerprint,
			int deltaInUnits) {
		super(id);

		CCSMAssert.isNotNull(location);

		this.cloneClass = cloneClass;

		// We use the Java string pool here for because:
		// - during clone detection, many clones can be created
		// - all fingerprints of non-gapped clones in same clone class are equal
		// (but created as different instances)
		this.fingerprint = fingerprint.intern();

		this.location = location;
		this.startUnitIndexInElement = startUnitIndexInElement;
		this.lengthInUnits = lengthInUnits;
		this.deltaInUnits = deltaInUnits;

		CCSMPre.isTrue(
				location.getRawEndOffset() >= location.getRawStartOffset(),
				"Length must not be negative: " + this);

		if (cloneClass != null) {
			cloneClass.add(this);
		}
	}

	/**
	 * Two clones are considered equal, if they describe the same region of code
	 * in the same element with the same gaps.
	 */
	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Clone)) {
			return false;
		}

		Clone other = (Clone) obj;

		// compare source
		if (!location.getUniformPath().equals(other.location.getUniformPath())) {
			return false;
		}

		// compare start units
		if (getStartUnitIndexInElement() != other.getStartUnitIndexInElement()) {
			return false;
		}

		// compare length in units
		if (getLastUnitInElement() != other.getLastUnitInElement()) {
			return false;
		}

		// compare gaps
		if (gapCount() != other.gapCount()) {
			return false;
		}
		for (int i = 0; i < gapCount(); ++i) {
			if (!gaps.get(i).equalsStartEnd(other.gaps.get(i))) {
				return false;
			}
		}

		// if code reaches here, clones are equal
		return true;
	}

	/**
	 * Returns the hash code based on the uniform path, the start and length.
	 * Gap information is ignored.
	 */
	@Override
	public int hashCode() {
		return (location.getUniformPath().hashCode() * 13 + getStartUnitIndexInElement())
				* 17 + getLengthInUnits();
	}

	/** {@link CloneClass} this clone belongs to */
	public CloneClass getCloneClass() {
		return cloneClass;
	}

	/**
	 * Fingerprint of clone. A clone fingerprint characterizes the piece of
	 * cloned code after normalization. All ungapped clones inside a single
	 * clone class have the same fingerprint.
	 */
	public String getFingerprint() {
		return fingerprint;
	}

	/** Position of the first unit of the clone in its element */
	public int getStartUnitIndexInElement() {
		return startUnitIndexInElement;
	}

	/** Length of the clone in units. */
	public int getLengthInUnits() {
		return lengthInUnits;
	}

	/** Position of the last unit of the clone in its element */
	public int getLastUnitInElement() {
		return getStartUnitIndexInElement() + getLengthInUnits() - 1;
	}

	/** Edit distance to first clone in clone class */
	public int getDeltaInUnits() {
		return deltaInUnits;
	}

	/** Returns the clone's location. */
	public TextRegionLocation getLocation() {
		return location;
	}

	/** Returns the uniform path of this clone's location. */
	public String getUniformPath() {
		return location.getUniformPath();
	}

	/** Determines whether the clone contains gaps */
	public boolean containsGaps() {
		return gaps != null && gaps.size() > 0;
	}

	/** Return number of gaps, or 0, if clone has no gaps */
	public int gapCount() {
		if (gaps == null) {
			return 0;
		}
		return gaps.size();
	}

	/**
	 * Returns a list of the gap positions as raw start end offsets (absolute
	 * offsets in the element), or empty list, if clone has no gaps.
	 */
	public UnmodifiableList<Region> getGapPositions() {
		if (gaps == null) {
			return CollectionUtils.emptyList();
		}

		return CollectionUtils.asSortedUnmodifiableList(gaps);
	}

	/** Adds a gap position. */
	public void addGap(Region gapRegion) {
		if (gaps == null) {
			gaps = new ArrayList<Region>();
		}
		gaps.add(gapRegion);
	}

	/** Set delta size in units */
	public void setDeltaInUnits(int deltaInUnits) {
		this.deltaInUnits = deltaInUnits;
	}

	/** String representation of the essential clone data. */
	@Override
	public String toString() {
		return "Clone [" + location.toLocationString() + "]";
	}

	/** Sets the clone class. Only called from CloneClass. */
	/* package */void setCloneClass(CloneClass cloneClass) {
		this.cloneClass = cloneClass;
	}
}

class CloneClass extends KeyValueStoreBase
{

    /** The length of the clone class (number of units).
        * @var int */
	private $normalizedLength;

    /** A list containing all clones of a class.
     * Hash table and linked list implementation of the Set interface, with predictable iteration order. This implementation differs from HashSet in that it maintains a doubly-linked list running through all of its entries. This linked list defines the iteration ordering, which is the order in which elements were inserted into the set (insertion-order). Note that insertion order is not affected if an element is re-inserted into the set. (An element e is reinserted into a set s if s.add(e) is invoked when s.contains(e) would return true immediately prior to the invocation.) 
     * @var Clone[] */
	private final $clones = [];

	/**
	 * Create a new clone class with a given id
	 * 
	 * @param int $normalizedLength Length of the clones (in units) in this clone class
	 */
    public function __construct(int $normalizedLength, int $id)
    {
        parent::__construct($id);
		$this->normalizedLength = $normalizedLength;
	}

	/**
	 * The normalized length of all clones in this class in units and thereby
	 * the length of this class.
	 */
    public function getNormalizedLength(): int
    {
		return $this->normalizedLength;
	}

	/** Set normalized length */
    public function setNormalizedLength(int $normalizedLength): void
    {
		$this->normalizedLength = $normalizedLength;
	}

	/** Computes and returns the fingerprint for this clone class */
    public function getFingerprint(): string
    {
		return CloneUtils::createFingerprint($this->getClones());
	}

    /** Returns the clones of this clone class.
        * @return Clone[] */
    public function getClones(): array
    {
		return $this->clones;
	}

	/** The size (number of clones) of this clone class. */
    public function size(): int
    {
		return count($this->clones);
	}

	/** Two clone classes are equal, if their fingerprints are equal */
    public function equals(object $obj): bool
    {
		if (!($obj instanceof CloneClass)) {
			return false;
		}

		return $this->getFingerprint() === $obj->getFingerprint();
	}

	/** Returns hash code of fingerprint */
	@Override
	public int hashCode() {
		return getFingerprint().hashCode();
	}

	/** Returns the number of gaps in the clone with the most gaps */
	public int getMaxGapNumber() {
		int gapCount = 0;
		for (Clone clone : getClones()) {
			if (gapCount < clone.gapCount()) {
				gapCount = clone.gapCount();
			}
		}
		return gapCount;
	}

	/** Returns sum of gaps contained in all clones */
	public int getGapCount() {
		int gapCount = 0;
		for (Clone clone : getClones()) {
			gapCount += clone.gapCount();
		}
		return gapCount;
	}

	/**
	 * Adds a clone to this class.
	 * <p>
	 * No sanity check is performed that makes sure that a clone really belongs
	 * to a clone class in order to allow different clone detection approaches
	 * to form clone classes for different notions of similarity
	 */
	public void add(Clone clone) {
		boolean cloneIsInOtherClass = clone.getCloneClass() != this
				&& clone.getCloneClass() != null;
		if (cloneIsInOtherClass) {
			clone.getCloneClass().remove(clone);
		}

		clones.add(clone);
		clone.setCloneClass(this);
	}

	/** Removes a clone */
	public void remove(Clone clone) {
		if (clones.remove(clone)) {
			clone.setCloneClass(null);
		}
	}
}

class MultiplexingCloneClassesCollection {

    /** Underlying collections */
    private final List<Collection<CloneClass>> collections = new ArrayList<Collection<CloneClass>>();

    /** Adds a clone class to all collections */
    public void add(CloneClass cloneClass) {
    for (Collection<CloneClass> collection : collections) {
        collection.add(cloneClass);
    }
    }

    /**
     * Returns a list with all clones in the contained collections. The list
     * is sorted by normalized length and contains no duplicates.
     */
    public List<CloneClass> getCloneClasses() {
    Set<CloneClass> resultSet = new HashSet<CloneClass>();

    for (Collection<CloneClass> boundedCollection : collections) {
        resultSet.addAll(boundedCollection);
    }

    return CollectionUtils.sort(resultSet,
        ECloneClassComparator.NORMALIZED_LENGTH);
    }

    /** Add a collection */
    public void addCollection(Collection<CloneClass> collection) {
    collections.add(collection);
    }
}

class CloneConsumer
{

    /** List in which the created clone classes are stored */
    private final MultiplexingCloneClassesCollection results = new MultiplexingCloneClassesCollection();

    /**
     * Creates a ICloneConsumer that writes the {@link CloneClass}es it
     * creates into the given set
     */
    public CloneConsumer() {
    if (top == INFINITE) {
        results.addCollection(new ArrayList<CloneClass>());
    } else {
        results.addCollection(boundedCollection(NORMALIZED_LENGTH));
        results.addCollection(boundedCollection(CARDINALITY));
        results.addCollection(boundedCollection(VOLUME));
    }
    }

    /** Creates {@link BoundedPriorityQueue} */
    private BoundedPriorityQueue<CloneClass> boundedCollection(
        ECloneClassComparator dimension) {
        return new BoundedPriorityQueue<CloneClass>(top, dimension);
    }

    /** {@link CloneClass} currently being filled */
    protected CloneClass currentCloneClass;

    /** Start new clone class */
    @Override
        public void startCloneClass(int normalizedLength) {
        currentCloneClass = new CloneClass(normalizedLength,
            idProvider.provideId());
    }

    /** Adds a clone to the current {@link CloneClass} */
    @Override
        public Clone addClone(int globalPosition, int length)
        throws ConQATException {
        // compute length of clone in lines
        Unit firstUnit = units.get(globalPosition);
        Unit lastUnit = units.get(globalPosition + length - 1);
        List<Unit> cloneUnits = units.subList(globalPosition,
            globalPosition + length);

        ITextElement element = resolveElement(firstUnit
            .getElementUniformPath());
        int startUnitIndexInElement = firstUnit.getIndexInElement();
        int endUnitIndexInElement = lastUnit.getIndexInElement();
        int lengthInUnits = endUnitIndexInElement - startUnitIndexInElement
            + 1;
        CCSMAssert.isTrue(lengthInUnits >= 0, "Negative length in units!");
        String fingerprint = createFingerprint(globalPosition, length);

        Clone clone = new Clone(idProvider.provideId(), currentCloneClass,
            createCloneLocation(element,
            firstUnit.getFilteredStartOffset(),
            lastUnit.getFilteredEndOffset()),
            startUnitIndexInElement, lengthInUnits, fingerprint);

        if (storeUnits) {
            CloneUtils.setUnits(clone, cloneUnits);
        }

        currentCloneClass.add(clone);

        return clone;
    }

    /** Creates the location for a clone. */
    private TextRegionLocation createCloneLocation(ITextElement element,
        int filteredStartOffset, int filteredEndOffset)
        throws ConQATException {
        int rawStartOffset = element
            .getUnfilteredOffset(filteredStartOffset);
        int rawEndOffset = element.getUnfilteredOffset(filteredEndOffset);
        int rawStartLine = element
            .convertUnfilteredOffsetToLine(rawStartOffset);
        int rawEndLine = element
            .convertUnfilteredOffsetToLine(rawEndOffset);

        return new TextRegionLocation(element.getLocation(),
            element.getUniformPath(), rawStartOffset, rawEndOffset,
            rawStartLine, rawEndLine);
    }

    /** Determine element for unit */
    protected ITextElement resolveElement(String elementUniformPath) {
    return uniformPathToElement.get(elementUniformPath);
    }

    /** Create fingerprint for current clone */
    protected String createFingerprint(int globalPosition, int length) {
    StringBuilder fingerprintBase = new StringBuilder();
    for (int pos = globalPosition; pos < globalPosition + length; pos++) {
        fingerprintBase.append(units.get(pos).getContent());
    }
    return Digester.createMD5Digest(fingerprintBase.toString());
    }

    /** Check constraints */
    @Override
        public boolean completeCloneClass() throws ConQATException {
        boolean constraintsSatisfied = constraints
            .allSatisfied(currentCloneClass);

        if (constraintsSatisfied) {
            results.add(currentCloneClass);
        }

        return constraintsSatisfied;
    }

    /** Return list containing all retained clone classes */
    public List<CloneClass> getCloneClasses() {
    return results.getCloneClasses();
    }
}

/** Collection that adds {@link CloneClass} to all contained collections */
private class MultiplexingCloneClassesCollection {

/** Underlying collections */
private final List<Collection<CloneClass>> collections = new ArrayList<Collection<CloneClass>>();

/** Adds a clone class to all collections */
public void add(CloneClass cloneClass) {
for (Collection<CloneClass> collection : collections) {
    collection.add(cloneClass);
}
}

/**
 * Returns a list with all clones in the contained collections. The list
 * is sorted by normalized length and contains no duplicates.
 */
public List<CloneClass> getCloneClasses() {
Set<CloneClass> resultSet = new HashSet<CloneClass>();

for (Collection<CloneClass> boundedCollection : collections) {
    resultSet.addAll(boundedCollection);
}

return CollectionUtils.sort(resultSet,
    ECloneClassComparator.NORMALIZED_LENGTH);
}

/** Add a collection */
public void addCollection(Collection<CloneClass> collection) {
collections.add(collection);
}
}
