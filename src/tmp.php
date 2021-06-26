<?php

/**
 * Base class for locations. Locations are immutable and thus return this at
 * deep cloning.
 * 
 * @author $Author: hummelb $
 * @version $Rev: 43290 $
 * @ConQAT.Rating GREEN Hash: 82BEB1645BABD9B7DBA7D15166C05209
 */
class ElementLocation
{

	/** Version used for serialization. */
	private static $serialVersionUID = 1;

    /** The location (see {@link #getLocation()}).
        * @var string */
	private $location;

    /** The uniform path (see {@link #getUniformPath()}).
        * @var string */
	private $uniformPath;

	/** Constructor. */
    public function __construct (string $location, string $uniformPath)
    {
		$this->location = $location;
		$this->uniformPath = $uniformPath;
	}

	/**
	 * Get a string that identifies the location of the element, e.g. a file
	 * system path. This location is specific to the running analysis, i.e.
	 * depends on time and the concrete machine ConQAT is running on.
	 */
    public function getLocation(): string
    {
		return $this->location;
	}

	/**
	 * Returns the uniform path. This is an artificial path that uniquely
	 * defines a resource across machine boundaries. This should be used for
	 * persisted information.
	 */
    public function getUniformPath(): string
    {
		return $this->uniformPath;
	}

    public function deepClone(): self
    {
		return $this;
	}

	/**
	 * Returns a single line description of the location that is meaningful to
	 * the user.
	 */
    public function toLocationString(): string
    {
		return getUniformPath();
	}

    public function __toString(): string
    {
		return $this->toLocationString();
	}
}

/**
 * This class denotes a region of text in an element.
 * 
 * <b>Context:</b> Due to the way ConQAT deals with text, the class is a little
 * bit more complex than expected. First, in ConQAT all text is normalized to
 * use Unix style line endings (regardless of the line endings in the file).
 * Second, ConQAT may apply filters, i.e. the internal (filtered) text
 * representation may be different from the (raw) text in the file.
 * Additionally, a location is best described by character offsets into the
 * string, while a user typically expects line numbers. Conversion between all
 * these representations is easy, as long as ConQAT internal representation is
 * available. Without it, conversion is not possible.
 * 
 * <b>Rationale:</b> When findings are reported to a user, the raw offsets
 * and/or lines should be used, as these are more meaningful (visible in other
 * editors as well). Also for persisting in a report, the raw positions are
 * preferred, as the filtered ones depend on the ConQAT configuration, while raw
 * offsets are independent of filter configuration. When working with findings
 * within ConQAT, typically the filtered positions are needed, as most
 * processors also work on the filtered representation. However, in such a case
 * the corresponding element is typically available and thus conversion to the
 * filtered representation is easy.
 * 
 * <b>Implementation:</b> The finding (as well as the findings report) only
 * stores raw positions. While the offsets would be sufficient, we also store
 * line numbers to be able to provide meaningful user output. Filtered positions
 * are not stored, but are made available via utility methods in the resource
 * bundle. All fields are mandatory, i.e., it is not allowed to fill any
 * position entry with invalid data (contrary to the old CodeRegionLocation,
 * where -1 could be used to denote missing information).
 * 
 * @author $Author: goede $
 * @version $Rev: 41698 $
 * @ConQAT.Rating GREEN Hash: 17529D0E53A92D53C5A9E732B5BA55C8
 */
class TextRegionLocation extends ElementLocation
{

	/** Version used for serialization. */
	private static $serialVersionUID = 1;

	/**
	 * The absolute start position of the region in the (raw) text (zero based,
	 * inclusive).
	 */
	private $rawStartOffset = 0;

	/**
	 * The absolute end position in the (raw) text (zero based, inclusive).
	 */
	private $rawEndOffset = 0;

	/**
	 * The line corresponding to {@link #rawStartOffset} (one-based, inclusive).
	 */
	private $rawStartLine = 0;

	/**
	 * The line corresponding to {@link #rawEndOffset} (one-based, inclusive).
	 */
	private $rawEndLine = 0;

	/** Constructor. */
    public function __construct(
        string $location,
        string $uniformPath,
        int $rawStartOffset,
        int $rawEndOffset,
        int $rawStartLine,
        int $rawEndLine
    ) {
        parent::__construct($location, $uniformPath);

		assert($rawStartOffset <= $rawEndOffset, "Start offset may not be after end offset.");
		assert($rawStartLine <= $rawEndLine, "Start line may not be after end line.");

		$this->rawStartOffset = $rawStartOffset;
		$this->rawEndOffset = $rawEndOffset;
		$this->rawStartLine = $rawStartLine;
		$this->rawEndLine = $rawEndLine;
	}

	/**
	 * Returns the absolute start position of the region in the (raw) text (zero
	 * based, inclusive).
	 */
    public function getRawStartOffset(): int
    {
		return $this->rawStartOffset;
	}

	/**
	 * Returns the absolute end position in the (raw) text (zero based,
	 * inclusive).
	 */
    public function getRawEndOffset(): int
    {
		return $this->rawEndOffset;
	}

	/**
	 * Returns the line corresponding to {@link #getRawStartOffset()}
	 * (one-based, inclusive).
	 */
    public function getRawStartLine(): int
    {
		return $this->rawStartLine;
	}

	/**
	 * Returns the line corresponding to {@link #getRawEndOffset()} (one-based,
	 * inclusive).
	 */
    public function getRawEndLine(): int
    {
		return $this->rawEndLine;
	}

	/**
	 * <p>
	 * This includes the start and end line which is typically sufficient for
	 * debugging and showing to a user.
	 */
    public function toLocationString(): string
    {
		return parent->toLocationString() . ":" . $this->rawStartLine . "-" . $this->rawEndLine;
	}
}

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

class Clone extends KeyValueStoreBase
{
    /** {@link CloneClass} this clone belongs to
        * @param CloneClass */
	private $cloneClass;

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
    public function setCloneClass(CloneClass $cloneClass): void
    {
		$this->cloneClass = $cloneClass;
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
	private $clones = [];

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
    public function hashCode(): int
    {
		return md5($this->getFingerprint());
	}

	/** Returns the number of gaps in the clone with the most gaps */
    public function getMaxGapNumber(): int
    {
		$gapCount = 0;
        foreach ($this->getClones() as $clone) {
			if ($gapCount < $clone->gapCount()) {
				$gapCount = $clone->gapCount();
			}
		}
		return $gapCount;
	}

	/** Returns sum of gaps contained in all clones */
    public function getGapCount(): int
    {
		int $gapCount = 0;
        foreach ($this->getClones() as $clone) {
			$gapCount += $clone->gapCount();
		}
		return $gapCount;
	}

	/**
	 * Adds a clone to this class.
	 * <p>
	 * No sanity check is performed that makes sure that a clone really belongs
	 * to a clone class in order to allow different clone detection approaches
	 * to form clone classes for different notions of similarity
	 */
    public function add(Clone $clone): void
    {
        $cloneIsInOtherClass =
               $clone->getCloneClass()->hashCode() !== $this->hashCode()
            && $clone->getCloneClass() !== null;
		if ($cloneIsInOtherClass) {
			$clone->getCloneClass()->remove($clone);
		}

		$this->clones[] = $clone;
		$clone->setCloneClass($this);
	}

	/** Removes a clone */
    public function remove(Clone $clone): void
    {
		if ($this->removeClone($clone)) {
			$clone->setCloneClass(null);
		}
	}

    /**
     * @return bool Returns true if $cloneSearch was removed from clones.
     */
    public function removeClone($cloneSearch): bool
    {
        $key = $this->findKeyOfClone($cloneSearch);
        if ($key !== null) {
            unset($this->clones[$key]);
            return true;
        }
        return false;
    }

    public function findKeyOfClone(Clone $cloneSearch): ?int
    {
        foreach ($this->getClones() as $i => $clone) {
            if ($clone->equals($cloneSearch)) {
                return $i;
            }
        }
        return null;
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
